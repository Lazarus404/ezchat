%%%---------------------------------------------------------------------------
%%% @doc Root application stub. Used to kick off the application supervisor.
%%%      Start with : -mnesia dir '"/Users/leesylvester/tmp/Mnesia.node2"' 
%%%      -sname ezchat2 -port <portnum>
%%%      add -cluster '<firstnode>' if you wish to join node to an existing
%%%      cluster.
%%% @end
%%%---------------------------------------------------------------------------

-module(ezchat_app).
-author('Lee Sylvester <lee.sylvester@gmail.com>').

-behaviour(application).

%% API.
-export([start/2, stop/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%% Kicks of the application supervisor.
start(_Type, _Args) ->
	{ok, _Xpid} = ezchat_sup:start_link().

%% Stops the application (actually handled by OTP).
stop(_State) ->
	ok.