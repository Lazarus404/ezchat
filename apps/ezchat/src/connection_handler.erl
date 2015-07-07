%%%---------------------------------------------------------------------------
%%% @doc Messaging Cowboy handler. Handles websocket connections and
%%%      throughput.
%%% @end
%%%---------------------------------------------------------------------------

-module(connection_handler).
-author('Lee Sylvester <lee.sylvester@gmail.com>').

-include("connection.hrl").

-export([init/2, websocket_handle/3, websocket_info/3, terminate/2]).

%% Sets up initial connection requirements. This includes
%% verifying the current user and adding his/her details
%% to the distributed store.
init(Req, Opts) ->
	erlang:display("Connection attempt made"),
	case connection_utils:parse_body(Req) of
		{ok, Room, Uname} ->
			connection_store:insert(self(), Uname, Room),
			connection_utils:request_peers(self(), Uname, Room),
			connection_utils:notify_peer_connection(Uname, Room),
			DateTime = calendar:local_time(),
			State = #messaging_user{pid = self(), username = Uname, room = Room, datetime = DateTime},
			{cowboy_websocket, Req, State};
		R -> R
	end.

%% Handles incoming data from the websocket.
websocket_handle({text, <<"ping">>}, Req, State) ->
	{ok, Req, State};
websocket_handle({text, Data}, Req, State) ->
	connection_utils:dispatch_message(Data, State#messaging_user.username, State#messaging_user.room),
	{ok, Req, State};
websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

%% Handles messages passed directly to this process (ie. via PID messaging).
websocket_info(Info, Req, State) ->
	{reply, {text, Info}, Req, State}.
% websocket_info(_Info, Req, State) ->
% 	{ok, Req, State}.

%% Handles cleanup when this process dies.
terminate(_Req, State) ->
	{Date, Time} = State#messaging_user.datetime,
	connection_utils:notify_peer_removed(State#messaging_user.username, State#messaging_user.room),
	connection_store:delete(self()),
	ezchat_db_store:log_usage(binary_to_list(State#messaging_user.room), integer_to_list(State#messaging_user.bytes), Date, Time, binary_to_list(State#messaging_user.username)),
	ok.