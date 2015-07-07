%%%---------------------------------------------------------------------------
%%% @doc Message queue server process supervisor.
%%% @end
%%%---------------------------------------------------------------------------

-module(mq_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% Sets up supervisor properties
init([]) ->
    Emitter = {mq_server_emitter, {mq_server_emitter, start_link, []},
              permanent, 2000, worker, [mq_server_emitter]},
    Receiver = {mq_server_receiver, {mq_server_receiver, start_link, []},
              permanent, 2000, worker, [mq_server_receiver]},
    Children = [Emitter, Receiver],
    RestartStrategy = {one_for_one, 5, 600},
    {ok, {RestartStrategy, Children}}.