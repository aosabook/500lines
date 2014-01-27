-module(torrent_client_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callback
%% ===================================================================

init([]) ->
    ok = application:ensure_started(inets),
    ok = application:ensure_started(crypto),
    ok = application:ensure_started(gproc),

    Rand = crypto:rand_bytes(12),
    ClientID = <<"-ET0000-", Rand/binary>>,
    ok = application:set_env(torrent_client, peer_id, ClientID),

    {ok, { {simple_one_for_one, 5, 10}, [?CHILD(torrent_client, worker)]} }.

