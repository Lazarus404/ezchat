%%%---------------------------------------------------------------------------
%%% @doc Returns a URL to a websocket endpoint.
%%% @end
%%%---------------------------------------------------------------------------

-module(ping_handler).
-author('Lee Sylvester <lee.sylvester@gmail.com>').

%% API functions.
-export([init/2]).

%% Default Cowboy initialise function.
init(Req, Opts) ->
	erlang:display("ping"),
	Reply = cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain; charset=utf-8">>}
	], <<"pong">>, Req),
	{ok, Reply, Opts}.