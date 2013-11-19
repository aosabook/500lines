-module(torrent_file).

-include("torrent.hrl").
-include_lib("kernel/include/file.hrl").

-behavior(gen_server).
-export([init/1,terminate/2,handle_call/3,handle_cast/2,handle_info/2]).
-export([download/1,downloaded/2,find/1,piece_offset/2,piece_sha/2,piece_length/2]).
-export([verify_piece/3]).

-record(state, { info       :: #info{},
                 peers = [] :: [pid()],
                 have = [],
                 file,
                 missing = [],
                 complete = false :: boolean(),
                 tracker_timer,
                 blocks :: ets:tid()
                 }).

download(TorrentFile) ->
    ok = application:ensure_started(torrent_client),
    Info = read_torrent_file(TorrentFile),
    case torrent_file:find(Info#info.info_hash) of
        {ok, PID} ->
            {ok, PID};
        false ->
            gen_server:start_link(?MODULE, [Info], [])
    end.

find(InfoHash) ->
    case ets:lookup(torrent_owners, InfoHash) of
        [{InfoHash,PID}] -> {ok, PID};
        [] -> false
    end.

downloaded(PID, Index) ->
    gen_server:cast(PID, {downloaded, Index}).

init([Info]) ->
    % erlang:process_flag(trap_exit, true),
  Blocks = ets:new(torrent_blocks, [public,ordered_set]),
  true = ets:insert(torrent_owners, {Info#info.info_hash, self()}),
  true = ets:insert(torrent_stats, {Info#info.info_hash, 0, 0}),
  {ok, State=#state{ missing=Missing }} = init_download_file(#state{ info=Info, blocks=Blocks }),
  case Missing of
    [] -> {stop, {shutdown, file_is_complete}};
    _  -> track(started, State)
  end.

terminate(_Reason, #state{ info=Info }) ->
    ets:delete(torrent_owners, Info#info.info_hash),
    ok.

handle_call(_Call,_,State) -> {stop, {error, unexpected_call, _Call}, State}.

handle_cast({downloaded, Index}, State=#state{ info=Info, peers=Peers, have=Have, missing = Missing }) ->
    Missing2 = ordsets:del_element(Index, Missing),
    Have2 = ordsets:add_element(Index, Have),
    io:format("**** got ~p, now have ~p / ~p~n", [Index, ordsets:size(Have2), Info#info.num_pieces]),
    case Missing2 == [] of
      true  -> [ erlang:exit(PeerPID, shutdown) || PeerPID <- Peers ],
               io:format("download done!~n"),
               {stop, {shutdown, file_is_complete}, State};
      false -> [ torrent_peer:coordinator_have(PeerPID, Index) || PeerPID <- Peers ],
               {noreply, State#state{ have = Have2, missing = Missing2}}
    end.

handle_info({'DOWN', _, process, PID, Info}, State) ->
    io:format("peer ~p died ~p~n", [PID, Info]),
    {noreply, State#state{ peers=lists:delete(PID, State#state.peers) }};

handle_info(update_tracker, State) ->
    {ok, State2} = track(empty, State),
    {noreply, State2}.

track(Event, State=#state{ info=#info{ piece_length=PieceLength, tracker_url=TrackerURL, info_hash=InfoHash }, missing=Missing }) ->
  {ok, #client{ peer_id=ClientID, port=Port }} = torrent_client:client_info(),
    Stats = ets:match(torrent_stats, {{InfoHash, '_'}, '$1', '$2'} ),
    [Uploaded, Downloaded] = lists:foldl(fun([Up, Down], [Up0, Down0]) -> [Up+Up0, Down+Down0] end, [0,0], Stats),
    RequestURL = lists:append([binary_to_list(TrackerURL),
                               "?info_hash=", url_encode(binary_to_list(InfoHash)),
                               "&peer_id=", url_encode(binary_to_list(ClientID)),
                               "&port=", erlang:integer_to_list(Port),
                               "&uploaded=", erlang:integer_to_list(Uploaded),
                               "&compact=1",
                               "&downloaded=", erlang:integer_to_list(Downloaded),
                               "&left=", erlang:integer_to_list(length(Missing)*PieceLength),
                               "&event=", atom_to_list(Event)]),
    io:format("track: ~s~n", [RequestURL]),
    case httpc:request(get, {RequestURL, []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            {ok, Response} = bencode:decode(Body),
            io:format("track response:~p~n", [Response]),
            case bencode:find(Response, <<"failure reason">>) of
                {ok, Failure} ->
                    {error, Failure};
                false ->
                    case bencode:find(Response, <<"interval">>) of
                        {ok, Interval} -> Timer=erlang:send_after(Interval * 1000, self(), update_tracker);
                        false          -> Timer=none
                    end,
                    case connect_peers(ClientID, State, bencode:get(Response, <<"peers">>)) of
                        Peers -> {ok, State#state{ tracker_timer=Timer, peers=Peers }}
                    end
            end;
        Error ->
            io:format("error ~p~n", [Error]),
            exit(tracker)
    end.

connect_peers(ClientID, State, Peers) when is_binary(Peers) ->
  [ connect({H3,H2,H1,H0}, Port, ClientID, State) || <<H3,H2,H1,H0,Port:16>> <= Peers ];
connect_peers(ClientID, State, Peers) ->
  [ connect(binary_to_list(bencode:get(Peer, <<"ip">>)),
    bencode:get(Peer, <<"port">>), ClientID, State) || Peer <- Peers].

connect(Host, Port, ClientID, #state{ info=Info, file=File, have=Have, blocks=Blocks }) ->
  PID = torrent_peer:start(Host, Port, ClientID, Info, File, Have, Blocks, self()),
  _ = erlang:monitor(process, PID),
  PID.

read_torrent_file(TorrentFileName) ->
    {ok, TorrentData} = file:read_file(TorrentFileName),
    {ok, TorrentDict} = bencode:decode(TorrentData),
    {ok, InfoDict}    = bencode:find(TorrentDict, info),
    {ok, Encoded}     = bencode:encode(InfoDict),
    {ok, Length}      = bencode:find(InfoDict, length),
    {ok, Name}        = bencode:find(InfoDict, name),
    FileName          = unicode:characters_to_list(Name, utf8),
    {ok, PieceLength} = bencode:find(InfoDict, 'piece length'),
    {ok, PieceHashes} = bencode:find(InfoDict, pieces),
    {ok, TrackerURL}  = bencode:find(TorrentDict, announce),
    PieceCount = byte_size(PieceHashes) div 20,
    InfoHash          = crypto:hash(sha, Encoded),
    #info{ info_hash=InfoHash, tracker_url=TrackerURL, name=FileName,
           total_length=Length, piece_length=PieceLength, num_pieces=PieceCount,
           piece_hashes=PieceHashes
         }.

%% create/find a file named <filename>.download with used to store downloaded pieces
init_download_file(State = #state{ info=#info{ name=FileName, total_length=Length, num_pieces=PieceCount } }) ->

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
                    Have = lists:flatten(rpc:pmap({?MODULE, verify_piece}, [F, State#state.info], All)),
                    {ok, State#state{ file=F, have=Have, missing=ordsets:subtract(All, Have) }};
                {error, enoent} ->
                    {ok, F} = file:open(DownloadName, [read,write]),
                    {ok, _} = file:position(F, {bof, Length}),
                    ok = file:truncate(F),
                    {ok, State#state{ file=F, missing=lists:seq(0, PieceCount-1) }}
            end
    end.

verify_piece(Index, File, Info) ->
    {ok, Data} = file:pread(File, piece_offset(Index, Info), piece_length(Index, Info)),
    case crypto:hash(sha, Data) == piece_sha(Index, Info) of
        true  -> Index;
        false -> []
    end.

piece_sha(N, #info{ piece_hashes=Hashes }) ->
    BeforeBytes = N*20,
    <<_:BeforeBytes/binary, SHA:20/binary, _/binary>> = Hashes,
    SHA.

piece_offset(N, #info{ piece_length=PL }) ->
    N*PL.

piece_length(P, #info{ piece_length=PL, num_pieces=N, total_length=TL })
  when P==N-1, (TL rem PL) /= 0 ->
    TL rem PL;
piece_length(_, #info{ piece_length=PL }) ->
    PL.

url_encode(List) -> lists:flatten(lists:map(fun encode/1, List)).
encode(N) when (N >= $0 andalso N =< $9); (N >= $a andalso N =< $z);(N >= $A andalso N =< $Z) -> N;
encode(N) -> [$%, hex(N bsr 4), hex(N band 15)].

hex(N) when N < 10 -> $0+N;
hex(N) -> $A+N-10.
