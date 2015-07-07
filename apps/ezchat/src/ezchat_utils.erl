%%%---------------------------------------------------------------------------
%%% @doc messaging related utils module.
%%% @end
%%%---------------------------------------------------------------------------

-module(ezchat_utils).
-author('Lee Sylvester <lee.sylvester@gmail.com>').

-export([message/3, error_message/3, set_request_cors/1]).

-export([out/4, get_env/3]).

%% Formats an outgoing message to an object the client expects
message(Path, Status, Msg) ->
	ezchat_utils:out(Path, Status, Msg, null).

%% Formats an outgoing error to an object the client expects
error_message(Path, Status, Error) ->
	ezchat_utils:out(Path, Status, null, Error).

% Forces a string to a bitstring
make_binary(Msg) when is_binary(Msg) ->
	Msg;
make_binary(Msg) when is_list(Msg) ->
	list_to_binary(Msg);
make_binary(Msg) ->
	mochijson2:encode(Msg).

%% Message formatter
%% -spec out( Path :: binary(), Status :: integer(), Data :: binary(), Error :: binary() ) -> Result :: binary().
out(Path, Status, Data, Error) ->
	mochijson2:encode({struct, [{ p, Path }, { s, Status }, { d, Data }, { e, Error }]}).

%% CORS helper. Ensures anyone can send requests.
set_request_cors(Req) ->
	Req2 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>, <<"GET, POST, OPTIONS">>, Req),
	Req3 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Headers">>, <<"Content-Type, X-Requested-With, Origin, Method">>, Req2),
	cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req3).

%% Returns a value from the config; or the passed default.
get_env(AppName, Key, Default) ->
	case application:get_env(AppName, Key) of
		undefined -> Default;
		{ok, Value} -> Value
	end.