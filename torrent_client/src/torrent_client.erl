-module(torrent_client).

-include("torrent.hrl").

-export([start_link/0, client_info/0, init/1, start/0]).

client_info() ->
  {ok, PeerID} = application:get_env(torrent_client, peer_id),
  {ok, IP} = application:get_env(torrent_client, ip),
  {ok, Port} = application:get_env(torrent_client, port),
  {ok, #client{ peer_id=PeerID, port=Port, ip=IP }}.

start() ->
  application:start(torrent_client).

start_link() ->
  ok = application:ensure_started(inets),
  ok = application:ensure_started(crypto),
    proc_lib:start_link(?MODULE, init, [self()]).

init(Caller) ->
    erlang:register(torrent_client, self()),

    ets:new(torrent_owners, [public,named_table]),
    ets:new(torrent_stats,  [public,named_table]),

    Rand = crypto:rand_bytes(12),
    ClientID = <<"-ET0000-", Rand/binary>>,
    20 = byte_size(ClientID),
    {ok, LocalName} = inet:gethostname(),
    HostName = application:get_env(?MODULE, host, LocalName),
    {ok, LocalAddr} = inet:getaddr(HostName, inet),
    {ok, ListenSock} =
        init_socket(LocalAddr, application:get_env(?MODULE, port, 6881)),
    {ok, {IP, Port}} = inet:sockname(ListenSock),

    ok = application:set_env(?MODULE, peer_id, ClientID),
    ok = application:set_env(?MODULE, ip, IP),
    ok = application:set_env(?MODULE, port, Port),

    proc_lib:init_ack(Caller, {ok, self()}),

    error_logger:info_msg("Started torrent_client ~p:~p (~p)~n", [IP, Port, ClientID]),

    accept_loop(ListenSock, ClientID).

init_socket(IP,Port) ->
    case gen_tcp:listen(Port, [{ip, IP}]) of
        {error, _} -> init_socket(IP, Port+1);
        {ok, _}=OK -> OK
    end.

accept_loop(ListenSock, ClientID) ->
    case torrent_protocol:accept(ListenSock, ClientID, fun(InfoHash)-> torrent_file:find(InfoHash) end) of
        {ok, {TSock, FilePID, PeerID}} ->
            gen_tcp:controlling_process(TSock, FilePID),
            torrent_file:new_peer(FilePID, TSock, PeerID),
            accept_loop(ListenSock, ClientID)
    end.
