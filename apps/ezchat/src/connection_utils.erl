%%%---------------------------------------------------------------------------
%%% @doc Distributed data store proxy. Used for storing and retrieving
%%%      user data, including users websocket process PID.
%%% @end
%%%---------------------------------------------------------------------------

-module(connection_utils).
-author('Lee Sylvester <lee.sylvester@gmail.com>').

-include("connection.hrl").

-export([
		parse_body/1,
		%parse_token/1,
		request_peers/3,
		notify_peer_connection/2,
		notify_peer_removed/2,
		dispatch_message/3,
		is_pid_alive/1
	]).

%% Parses a HTTP request by type. Currently, only GET is expected and supported.
parse_body(Req) ->
	{ok, cowboy_req:binding(room, Req), cowboy_req:binding(username, Req)}.

%% Returns all users within a given room, except for the user associated with the passed PID.
request_peers(Pid, Uname, Room) ->
	Users = {struct, [{users, get_room_users(Uname, Room)}]},
	mq_server_emitter:message_process(Pid, format_message(<<"peers">>, Uname, Room, Users)),
	%Pid ! format_message(<<"peers">>, Uname, Room, Users),
	ok.

%% Sends the passed username to all other members of his/her room as connected. 
notify_peer_connection(Uname, Room) ->
	notify_peers(Uname, Room, <<"peer_connected">>).

%% Sends the passed username to all other members of his/her room as disconnected. 
notify_peer_removed(Uname, Room) ->
	notify_peers(Uname, Room, <<"peer_removed">>).

%% Handles all-peer notification.
notify_peers(Uname, Room, Type) ->
	Users = get_room_user_records(Uname, Room),
	Msg = format_message(Type, Uname, Room, Uname),
	F = fun(U) ->
		Pid = U#messaging_user.pid,
		case is_pid_alive(Pid) of
			true -> mq_server_emitter:message_process(Pid, Msg);
			%true -> Pid ! Msg;
			_ -> connection_store:delete(Pid)
		end
	end,
	map(F, Users),
	ok.

%% Dispatches the message stored in the passed JSON to the target user specified in the JSON.
%% Only dispatches if the target user exists in the same room as the dispatcher.
%% See RTC client for JSON structure.
dispatch_message(Json, From, Room) ->
	{struct, Data} = mochijson2:decode(Json),
	Event = proplists:get_value(<<"eventName">>, Data),
	TargetUser = proplists:get_value(<<"targetUserId">>, Data),
	User = get_user(TargetUser, Room),
	erlang:display(User),
	F = fun(U) ->
		Pid = U#messaging_user.pid,
		case is_pid_alive(Pid) of
			% Could not use format_message as JSON encoding of passed messages may error (mochijson2 is not sophisticated enough).
			true -> mq_server_emitter:message_process(Pid, <<"{\"type\":\"", Event/binary, "\", \"userid\":\"", From/binary, "\", \"room\":\"", Room/binary, "\", \"message\":", Json/binary, "}">>);
			%true -> Pid ! <<"{\"type\":\"", Event/binary, "\", \"userid\":\"", From/binary, "\", \"room\":\"", Room/binary, "\", \"message\":", Json/binary, "}">>;
			_ -> connection_store:delete(Pid)
		end
	end,
	map(F, User),
	ok.

%% Helper functions

%% Formats messages into a common structure.
format_message(Type, From, Room, Msg) ->
	mochijson2:encode({struct, [
			{type, Type},
			{userid, From},
			{room, Room},
			{message, Msg}
		]}).

%% Returns a list of usernames for all users in a given room
get_room_users(_Uname, Room) ->
	%[U#messaging_user.username || U <- connection_store:select(Room, App, Dom), U#messaging_user.username /= Uname].
	[U#messaging_user.username || U <- connection_store:select(Room)].

%% Returns a list of user records for all users in a given room
get_room_user_records(Uname, Room) ->
	[U || U <- connection_store:select(Room), U#messaging_user.username /= Uname].
	%[U || U <- connection_store:select(Room, App, Dom)].

%% Returns a specific user or list duplicate users. User PIDs must be tested
%% to see if they're still live
get_user(Uname, Room) ->
	connection_store:select(Uname, Room).

%% Functional map helper
map(Fun, L) -> [Fun(X) || X <- L].

%% Checks to see if a given process, referenced by its PID, is still active.
is_pid_alive(Pid) when node(Pid) =:= node() ->
	is_process_alive(Pid);
is_pid_alive(Pid) ->
	case lists:member(node(Pid), nodes()) of
	false ->
		false;
	true ->
		case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
			true ->
				true;
			false ->
				false;
			{badrpc, _Reason} ->
				false
		end
	end.