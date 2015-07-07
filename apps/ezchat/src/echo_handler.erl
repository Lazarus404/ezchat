%%%---------------------------------------------------------------------------
%%% @doc Messaging Cowboy echo handler. Handles websocket echo connections for
%%%      use when testing with websockets.org etc.
%%% @end
%%%---------------------------------------------------------------------------

-module(echo_handler).
-author('Lee Sylvester <lee.sylvester@gmail.com>').

-include("connection.hrl").

-export([init/2, websocket_handle/3, websocket_info/3, terminate/2]).

%% Sets up initial connection requirements. This includes
%% verifying the current user and adding his/her details
%% to the distributed store.
init(Req, Opts) ->
	{cowboy_websocket, Req, []}.

%% Handles incoming data from the websocket.
websocket_handle(Data, Req, State) ->
	%self() ! Data,
	{reply, Data, Req, State}.

%% Handles messages passed directly to this process (ie. via PID messaging).
websocket_info(Info, Req, State) ->
	{reply, Info, Req, State}.

%% Handles cleanup when this process dies.
terminate(_Req, _State) ->
	ok.