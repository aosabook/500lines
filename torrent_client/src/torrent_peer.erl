-module(torrent_peer).

-behavior(plain_fsm).

-export([start/8]).
-export([piece_finished/2]).
-export([code_change/3]).

-include("torrent.hrl").
-include_lib("plain_fsm/include/plain_fsm.hrl").

-record(state, {
          manager                              :: pid(),
          sock                                 :: undefined|gen_tcp:socket(),
          blocks_tab                           :: ets:tid(),
          info                                 :: #torrent_info{},
          file                                 :: file:io_device(),
          im_choked           = true           :: boolean(),
          im_interested       = false          :: boolean(),
          peer_is_choked      = true           :: boolean(),
          peer_is_interested  = false          :: boolean(),
          have                = []             :: [non_neg_integer()],
          want                = []             :: [non_neg_integer()],
          inq                 = queue:new()    :: queue(),
          outq                = queue:new()    :: queue(),
          inflight            = ordsets:new()  :: ordsets:ordsets(),
          upload_allowance    = 0              :: integer(),
          timer                                :: reference()
}).

-define(MAX_INFLIGHT_REQUESTS, 30).
-define(UPLOAD_BYTES_PER_SECOND, 1024 * 50).

%% API

piece_finished(PeerPID, Index) -> PeerPID ! {finished, Index}.

%% gen_server logic

code_change(_OldVsn, State, _Extra) -> {ok, State, [{cont, fun main_loop/1}]}.

start(Host, Port, ClientID, Info, File, Have, Blocks, Manager) ->
  plain_fsm:spawn(?MODULE, fun()-> init(Host, Port, ClientID,
    #state{ info=Info, file=File, have=Have, manager=Manager, blocks_tab=Blocks} ) end).

init(Host, Port, ClientID, State0=#state{ info=Info=#torrent_info{ info_hash=InfoHash }, have=Have }) ->
  erlang:process_flag(trap_exit, true),
  case torrent_protocol:connect(Host, Port, ClientID, Info#torrent_info.info_hash) of
    {ok, {Sock, _PeerID}} ->
      random:seed(erlang:now()),
      gproc:add_local_counter({InfoHash, upload}, 0),
      gproc:add_local_counter({InfoHash, download}, 0),
      Timer = erlang:start_timer(5000+random:uniform(10000), self(), ten_sec_timer),
      State = State0#state{ sock=Sock, timer=Timer },
      ok = inet:setopts(Sock, [{active, once}, {packet, 4}]),
      case Have == [] of
        true  -> State2 = State;
        false -> {ok, State2} = send({bitfield, Have}, State)
      end,
      do_work(State2);
    Error ->
      exit({shutdown, Error})
  end.

main_loop(State=#state{ sock=Sock, info=Info, upload_allowance=Allowance, want=Want, have=Have }) ->
  Parent = plain_fsm:info(parent),
  receive
    {tcp, Sock, Packet} ->
       Message = torrent_protocol:decode_packet(Packet, Info),
       inet:setopts(Sock, [{active,once}]),
       handle_incoming(Message, State);
    {tcp_closed, Sock} ->
      exit({shutdown, tcp_closed});
    {tcp_error, Sock, Reason} ->
      exit({shutdown, {tcp_error, Reason}});

    {timeout, _, ten_sec_timer} ->
      Timer = erlang:start_timer(10000, self(), ten_sec_timer),
      {ok, State2} = send(keep_alive, State),
      do_work(State2#state{ upload_allowance=(10*?UPLOAD_BYTES_PER_SECOND)+min(0,Allowance), timer=Timer });

    {finished, Index} ->
      case ordsets:is_element(Index, Want) of
        true  -> {ok, State2} = cancel(Index, State);
        false -> {ok, State2} = send({have, Index}, State)
      end,
      do_work(State2#state{ have=ordsets:add_element(Index, Have), want=ordsets:del_element(Index, Want) });

    {system, From, Req} ->
      plain_fsm:handle_system_msg(From, Req, State, fun main_loop/1);
    {'EXIT', Parent, Reason} ->
      plain_fsm:parent_EXIT(Reason, State)
  end.

do_work(State) ->
  {ok, State1} = fill_outq(State),
  {ok, State2} = send_requests(State1),
  {ok, State3} = maybe_remove_interest(State2),
  {ok, State4} = maybe_unchoke_peer(State3),
  {ok, State5} = send_replies(State4),
  main_loop(State5).

cancel(Index, State=#state{ inflight=InFlight, blocks_tab=Blocks, info=Info }) ->
  ets:match_delete(Blocks, {{Info#torrent_info.info_hash, Index, '_'}, '_'}),
  ToCancel = ordsets:filter(fun({request, I, _, _}) -> I==Index end, InFlight),
  {ok, State1} = ordsets:fold(fun({request, I, O, L}, {ok, State0}) -> send({cancel, I, O, L}, State0) end,
    {ok, State},
    ToCancel),
  {ok, State1#state{ inflight=ordsets:subtract(InFlight, ToCancel),
                     outq=queue:filter(fun({request, I, _, _}) -> I == Index end, State1#state.outq) }}.

request_piece(Index, State=#state{ im_interested=false }) ->
  {ok, State2} = send({interest, true}, State),
  request_piece(Index, State2);
request_piece(Index, State=#state{ info=Info }) ->
  PieceLength = torrent_file:piece_length(Index, Info),
  BlockOffsets = lists:seq(0, PieceLength-1, ?BLOCK_SIZE),
  {Head, Tail} = lists:split(random:uniform(length(BlockOffsets)), BlockOffsets),
  OutQ = lists:foldl(fun(Offset,Q) ->
                        Length = min(?BLOCK_SIZE, PieceLength-Offset),
                        queue:in({request, Index, Offset, Length}, Q)
                     end,
                     State#state.outq,
                     Tail ++ Head),
  {ok, State#state{ outq=OutQ }}.

fill_outq(State=#state{ outq=OutQ, want=Want }) ->
    case (Want /= []) andalso queue:len(OutQ) < ?MAX_INFLIGHT_REQUESTS of
        true  -> request_piece(lists:nth(random:uniform(length(Want)), Want), State);
        false -> {ok, State}
    end.

%% if we're not choked, try to send up to MAX_INFLIGHT_REQUESTS requests
send_requests(State=#state{ im_choked=false, inflight=InFlight }) ->
    case ordsets:size(InFlight) < ?MAX_INFLIGHT_REQUESTS of
        true ->
            case queue:out(State#state.outq) of
                {{value, Message}, OutQ} ->
                    {ok, State2} = send(Message, State),
                    send_requests(State2#state{ outq=OutQ, inflight=ordsets:add_element(Message, InFlight) });
                {empty, _} ->
                    {ok, State}
            end;
        false ->
            {ok, State}
    end;
send_requests(State) ->
    {ok, State}.

%% ensure that we have set interest=false if we want nothing from this peer
maybe_remove_interest(State=#state{ im_interested=true, want=[] }) ->
    send({interest, false}, State);
maybe_remove_interest(State) ->
    {ok, State}.

%% unchoke peer if we have excess outbound bandwidth
maybe_unchoke_peer(State=#state{ peer_is_choked=true, upload_allowance=Allowance }) when Allowance > 0 ->
    send({choke, false}, State);
maybe_unchoke_peer(State) ->
    {ok, State}.

send_replies(State=#state{ peer_is_choked=false,
                           peer_is_interested=true,
                           upload_allowance=Allowance,
                           file=File, info=Info=#torrent_info{ info_hash=InfoHash } }) ->
    case Allowance > 0 of
        true ->
            case queue:out(State#state.inq) of
                {empty, _} ->
                    {ok, State};
                {{value, {requests, Index, Offset, Length}}, Q2} ->
                  {ok, Data} = file:pread(File, torrent_file:piece_offset(Index, Info) + Offset, Length),
                  gproc:update_counter({c,l,{InfoHash, upload}}, Length),
                  io:format("~p SENDING {block, ~p, ~p, ...}~n", [self(), Index, Offset]),
                  {ok, State2} = send({block, Index, Offset, Data},
                    State=#state{ inq=Q2, upload_allowance=Allowance-Length }),
                  send_replies(State2)
            end;
        false ->
            send({choke, true}, State)
    end;
send_replies(State) ->
    {ok, State}.

send({interest, Interest}=Message, State=#state{ sock=Sock, info=Info }) ->
    ok = gen_tcp:send(Sock, torrent_protocol:encode_packet(Message, Info)),
    {ok, State#state{ im_interested=Interest }};
send({choke, Choke}=Message, State=#state{ sock=Sock, info=Info }) ->
    ok = gen_tcp:send(Sock, torrent_protocol:encode_packet(Message, Info)),
    {ok, State#state{ peer_is_choked=Choke }};
send(Message, State=#state{ sock=Sock, info=Info }) ->
    ok = gen_tcp:send(Sock, torrent_protocol:encode_packet(Message, Info)),
    {ok, State}.

%% Handle incoming traffic

handle_incoming(Message, State=#state{ inflight=InFlight, inq=InQ, outq=OutQ, have=IHave, want=Want, info=Info }) ->
    State3 =
    case Message of
        {interest, IsInterested} ->
            State#state{ peer_is_interested=IsInterested };
        {choke, true}  ->
            State#state{ im_choked=true, inflight=[], outq=ordsets:fold(fun queue:in_r/2, OutQ, InFlight) };
        {choke, false} ->
            State#state{ im_choked=false };
        {bitfield, PeerHave} ->
            State#state{ want=ordsets:subtract(PeerHave, IHave) };
        {have, Index} ->
            case ordsets:is_element(Index, IHave) of
                true  -> State;
                false -> {ok, S} = request_piece(Index, State#state{ want=ordsets:add_element(Index, Want) }), S
            end;
        {request, _, _, _} when State#state.peer_is_choked ->
            State; % ignore
        {request, _, _, _}=Req ->
            State#state{ inq=queue:in(Req, InQ) };
        {cancel, Index, Offset, Length} ->
            State#state{ inq=queue:filter(fun(Req) -> Req =/= {request, Index, Offset, Length} end, InQ)};
        {block, Index, Offset, Data} ->
            BlockTab = State#state.blocks_tab,
            BlockLength = byte_size(Data),
            InfoHash = Info#torrent_info.info_hash,
            State2 = State#state{ inflight=ordsets:del_element({request,Index,Offset,BlockLength}, InFlight) },
            gproc:update_counter({c,l,{InfoHash, download}}, BlockLength),
                true = ets:insert(BlockTab,{{InfoHash,Index,Offset},Data}),
            PieceData = ets:match(BlockTab, {{InfoHash,Index,'_'}, '$1'}),
            case iolist_size(PieceData) == torrent_file:piece_length(Index, Info) of
                true ->
                    ets:match_delete(BlockTab, {{InfoHash,Index,'_'}, '_'}),
                    case torrent_file:piece_sha(Index, Info) == crypto:hash(sha, PieceData) of
                        true  ->
                            file:pwrite(State#state.file, torrent_file:piece_offset(Index, Info), PieceData),
                            torrent_client:downloaded(State#state.manager, Index),
                            State2#state{ want=ordsets:del_element(Index, Want) };
                        false ->
                            io:format("~p ** BAD! ~p~n", [self(), Index]),
                            State2
                    end;
                false ->
                    State2
            end;
        keep_alive ->
            State;
        {extended, _, _} ->
            State
    end,
    do_work(State3).

