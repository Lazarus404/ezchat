%%%---------------------------------------------------------------------------
%%% @doc Cowboy server process supervisor.
%%% @end
%%%---------------------------------------------------------------------------

-module(ezchat_sup).
-author('Lee Sylvester <lee.sylvester@gmail.com>').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
%% Helper macro for routing module name to server identifier
-define(SERVER, ?MODULE).


%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% Sets up supervisor properties
init([]) ->
	application:start(log4erl),
	log4erl:conf("priv/log4erl.conf"),
	Port = ezchat_server:ezchat_port(),
	XServer = {ezchat_server, {ezchat_server, start_link, [Port]},
				permanent, 2000, worker, [ezchat_server]},
	MqServer = {mq_sup, {mq_sup, start_link, []},
				permanent, 2000, worker, [mq_sup]},
	Children = [XServer, MqServer],
	RestartStrategy = {one_for_all, 10, 3600},
    {ok, {RestartStrategy, Children}}.

