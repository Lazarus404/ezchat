%%%---------------------------------------------------------------------------
%%% @doc Distributed data store proxy. Used for storing and retrieving
%%%      user data, including users websocket process PID.
%%% @end
%%%---------------------------------------------------------------------------

-module(connection_store).

-include("connection.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([
			init/0,
			insert/3,
			delete/1,
			lookup/1,
			select/1,
			select/2
		]).

-define(WAIT_FOR_TABLES, 5000).

%% Sets up and initialises the distributed data store for the current node.
init() ->
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	mnesia:start(),
	dynamic_db_init([]).%nodes()).

%% Inserts a user into the distributed store.
insert(Pid, Username, Room) ->
	mnesia:dirty_write(messaging_user, #messaging_user{
		pid = Pid,
		username = Username,
		room = Room
	}).

%% Deletes a user from the distributed store.
delete(Pid) ->
	case mnesia:dirty_read(messaging_user, Pid) of
		[#messaging_user{} = Record] ->
			mnesia:dirty_delete_object(Record);
		_ ->
			ok
	end.

%% Retrieves a user by PID.
lookup(Pid) ->
	case mnesia:dirty_read(messaging_user, Pid) of
		[{messaging_user, Pid}] ->
			case connection_utils:is_pid_alive(Pid) of
				true -> {ok, Pid};
				false -> {error, not_found}
			end;
		[] ->
			{error, not_found}
	end.

%% Retrieves a user (or more, should they exist) with a full users details.
select(undefined, Room) ->
	select(Room);
select(Username, Room) ->
	QH = qlc:q( [U || U <- mnesia:table(messaging_user),
		U#messaging_user.username == Username,
		U#messaging_user.room == Room
		], {unique, true}),
	F = fun() ->
		qlc:eval(QH)
	end,
	{atomic, Result} = mnesia:transaction(F),
	Result.
%% Retrieves one or more users by room, application and domain values.
select(Room) ->
	QH = qlc:q( [U || U <- mnesia:table(messaging_user),
		U#messaging_user.room == Room
		]),
	F = fun() ->
		qlc:eval(QH)
	end,
	{atomic, Result} = mnesia:transaction(F),
	Result.

%%=====================================================================
%% Internal Functions
%%=====================================================================

%% If no nodes are passed, then sets up tables in a fresh database 
%% instance. Otherwise, Mnesia is hooked up to the remote Mnesia
%% instances.
dynamic_db_init([]) ->
	mnesia:create_table(messaging_user,
		[
			{type,set},
			{record_name, messaging_user},
			{index, [username,room]},
			{attributes, record_info(fields, messaging_user)}
		]);
dynamic_db_init(Nodes) ->
	add_extra_nodes(Nodes).

%% Augments this nodes Mnesia instance to match Mnesia instance of other node.
add_extra_nodes([]) ->
	{ok, []};
	
add_extra_nodes([Node|T]) ->
	Name = string:str(atom_to_list(Node), "ezchat"),
	if
		Name > 0 ->
			case mnesia:change_config(extra_db_nodes, [Node]) of
				{ok, [Node]} ->
					mnesia:add_table_copy(schema, node(), ram_copies), % copy over general schema
					mnesia:add_table_copy(messaging_user, node(), ram_copies), % copy over specific table
					Tables = mnesia:system_info(tables), % get info on all tables
					mnesia:wait_for_tables(Tables, ?WAIT_FOR_TABLES); % wait on all tables to copy
				_ ->
					add_extra_nodes(T) % loop for all other nodes
			end;
		true ->
			add_extra_nodes(T)
	end.