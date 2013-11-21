-module(torrent_client).

-include("torrent.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("plain_fsm/include/plain_fsm.hrl").

-behavior(plain_fsm).
-export([init/1,terminate/2,code_change/3,start_link/1]).
-export([download/1,downloaded/2,verify_piece/3]).

%% Application callbacks
-behaviour(application).
-export([start/2, stop/1]).
start(_StartType, _StartArgs) ->
    torrent_client_sup:start_link().
stop(_State) ->
    ok.


-record(state, { info             :: #torrent_info{},
                 peers = []       :: [pid()],
                 have = []        :: ordsets:ordset(),
                 file             :: file:io_device(),
                 missing = []     :: ordsets:ordset(),
                 complete = false :: boolean(),
                 tracker_timer    :: reference(),
                 blocks           :: ets:tid(),
                 last = {erlang:now(), 0, 0}
                 }).

download(TorrentFile) ->
    ok = application:ensure_started(torrent_client),
    supervisor:start_child(torrent_client_sup, [TorrentFile]).

start_link(TorrentFile) ->
    Info = torrent_file:read(TorrentFile),
    case catch gproc:lookup_local_name(Info#torrent_info.info_hash) of
        undefined ->
            plain_fsm:start_opt(?MODULE, fun() -> init(Info) end, 60*1000, [link]);
        PID ->
            {ok, PID}
    end.

downloaded(PID, Index) ->
    PID ! {downloaded, Index}.

init(Info=#torrent_info{ info_hash=InfoHash }) ->
    % erlang:process_flag(trap_exit, true),
    true = gproc:add_local_name(InfoHash),
    gproc:add_local_aggr_counter({InfoHash, upload}),
    gproc:add_local_aggr_counter({InfoHash, download}),
    Blocks = ets:new(torrent_blocks, [public,ordered_set]),
    {ok, State=#state{ missing=Missing }} = init_download_file(#state{ info=Info, blocks=Blocks }),
    case Missing of
        [] -> exit({shutdown, file_is_complete});
        _  -> case track(started, State) of
                  {ok, Interval, Peers} ->
                      Timer=erlang:send_after(Interval * 1000, self(), update_tracker),
                      {ok, ClientID} = application:get_env(torrent_client, peer_id),
                      PeerPIDs = [ connect(Host, Port, ClientID, State) || {Host,Port} <- Peers ],
                      {reply, {ok, self()}, fun() -> main_loop(State#state{ peers=PeerPIDs, tracker_timer=Timer }) end }
              end
    end.

connect(Host, Port, ClientID, #state{ info=Info, file=File, have=Have, blocks=Blocks }) ->
  PID = torrent_peer:start(Host, Port, ClientID, Info, File, Have, Blocks, self()),
  _ = erlang:monitor(process, PID),
  PID.

terminate(_Reason, State) ->
  erlang:cancel_timer( State#state.tracker_timer ),
  track(stopped, State),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State, [{cont, fun main_loop/1}]}.

main_loop(State=#state{ info=Info, peers=Peers, have=Have, missing = Missing }) ->
    Parent = plain_fsm:info(parent),
    receive
        {downloaded, Index} ->
            {LastTime, LastUp, LastDown} = State#state.last,
            InfoHash   = Info#torrent_info.info_hash,
            Uploaded   = gproc:lookup_local_aggr_counter({InfoHash, upload}),
            Downloaded = gproc:lookup_local_aggr_counter({InfoHash, download}),
            State2=State#state{ missing = ordsets:del_element(Index, Missing),
                                have = ordsets:add_element(Index, Have),
                                last = {Time=erlang:now(), Uploaded, Downloaded}},
            PassedSecs = timer:now_diff(Time, LastTime) / 1000000,
            DeltaDownMB = (Downloaded-LastDown)/(1024*1024),
            DeltaUpMB = (Uploaded-LastUp)/(1024*1024),
            io:format("**** got piece #~p, now have ~p% (up ~p MB/s, down ~p MB/s), ~p peers~n",
                      [Index, 100*ordsets:size(State2#state.have) div Info#torrent_info.num_pieces,
                       DeltaUpMB/PassedSecs, DeltaDownMB/PassedSecs, length(Peers)]),
            case State2#state.missing == [] of
                true  ->
                    [ erlang:exit(PeerPID, shutdown) || PeerPID <- Peers ],
                    _ = file:rename(Info#torrent_info.name ++ ".download", Info#torrent_info.name),
                    io:format("download done!~n"),
                    track(stopped, State2),
                    exit({shutdown, file_is_complete});
                false ->
                    [ torrent_peer:piece_finished(PeerPID, Index) || PeerPID <- Peers ],
                    main_loop(State2)
            end;

        {'DOWN', _Ref, process, SomePID, _Reason} ->
            io:format("pid ~p exited with ~p\n", [SomePID, _Reason]),
            main_loop(State#state{ peers=lists:delete(SomePID, Peers) });

        update_tracker ->
            {ok, Interval, _} = track(empty, State),
            Timer=erlang:send_after(Interval * 1000, self(), update_tracker),
            main_loop(State#state{ tracker_timer = Timer });

        {system, From, Req} ->
            plain_fsm:handle_system_msg(From, Req, State, fun main_loop/1);
        {'EXIT', Parent, Reason} ->
            plain_fsm:parent_EXIT(Reason, State)
    end.


%%
%% Contact the tracker
%%

track(Event, #state{ info=Info=#torrent_info{ tracker_url=TrackerURL, info_hash=InfoHash }, missing=Missing }) ->
    {ok, ClientID} = application:get_env(torrent_client, peer_id),
    Uploaded   = gproc:lookup_local_aggr_counter({InfoHash, upload}),
    Downloaded = gproc:lookup_local_aggr_counter({InfoHash, download}),
    Left       = lists:sum([ torrent_file:piece_length(I, Info) || I <- Missing]),
    RequestURL = lists:append([binary_to_list(TrackerURL),
                               "?info_hash=", url_encode(InfoHash),
                               "&peer_id=", url_encode(ClientID),
                               "&port=6881", % integer_to_list(Port),
                               "&uploaded=", integer_to_list(Uploaded),
                               "&downloaded=", integer_to_list(Downloaded),
                               "&left=", integer_to_list(Left),
                               "&compact=1",
                               "&event=", atom_to_list(Event)]),
    case httpc:request(get, {RequestURL, []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            {ok, Response} = bencode:decode(Body),
            io:format("tracker response: ~p~n", [Response]),
            Peers = case bencode:get(Response, peers) of
                        BinPeers when is_binary(BinPeers) ->
                            [ {{H3,H2,H1,H0}, PeerPort} || <<H3,H2,H1,H0,PeerPort:16>> <= BinPeers ];
                        ListPeers when is_list(ListPeers) ->
                            [ {bencode:get(Peer, ip), bencode:get(Peer, port)} || Peer <- ListPeers]
                    end,
            {ok, bencode:get(Response, interval), Peers};
        {ok, {{_,Code,_}, _, Body}} ->
            {ok, Response} = bencode:decode(Body),
            case bencode:find(Response, 'failure reason') of
                {ok, Failure} ->
                    {error, Failure};
                error ->
                    {error, {code, Code}}
            end;
        Error ->
            Error
    end.

url_encode(Binary) -> lists:append([ hex(N) || <<N>> <= Binary ]).

hex(N) when N < 16  -> [$%, $0 | integer_to_list(N, 16)];
hex(N) when N < 256 -> [$% | integer_to_list(N, 16)].

%%
%% Initialize/Verify a file that we may have partially downloaded
%%

init_download_file(State = #state{ info=#torrent_info{ name=FileName, total_length=Length, num_pieces=PieceCount } }) ->
    case file:read_file_info(FileName) of
        {ok, #file_info{ size=Length }} ->
            {ok, F} = file:open(FileName, [read]),
            {ok, State#state{ file=F, missing=[], complete=true }};
        {error, enoent} ->
            DownloadName = FileName ++ ".download",
            case file:read_file_info(DownloadName) of
                {ok, #file_info{ size=Length }} ->
                    {ok, F}  = file:open(DownloadName, [read,write]),
                    All = lists:seq(0, PieceCount-1),
                    io:format("checking: ~p...", [FileName]),
                    Have = lists:flatten( [ verify_piece(Index, F, State#state.info) || Index <- All ] ),
                    io:format(" have ~p of ~p pieces~n", [length(Have), PieceCount]),
                    {ok, State#state{ file=F, have=Have, missing=ordsets:subtract(All, Have) }};
                {error, enoent} ->
                    {ok, F} = file:open(DownloadName, [read,write]),
                    {ok, _} = file:position(F, {bof, Length}),
                    ok = file:truncate(F),
                    {ok, State#state{ file=F, missing=lists:seq(0, PieceCount-1) }}
            end
    end.

verify_piece(Index, File, Info) ->
    {ok, Data} = file:pread(File, torrent_file:piece_offset(Index, Info), torrent_file:piece_length(Index, Info)),
    case crypto:hash(sha, Data) == torrent_file:piece_sha(Index, Info) of
        true  -> Index;
        false -> []
    end.
