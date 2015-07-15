%%%---------------------------------------------------------------------------
%%% @doc Starts Cowboy and Mnesia processes.
%%% @end
%%%---------------------------------------------------------------------------

-module(ezchat_server).
-author('Lee Sylvester <lee.sylvester@gmail.com>').

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("properties.hrl").

-define(SERVER, ?MODULE).

%% Starts a new process for this server (see init function).
start_link(Port) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%% Starts a new process for this server with default port value (see init function).
start_link() ->
	start_link(?DEFAULT_PORT).

%% Stops the current server process.
stop() ->
	gen_server:cast(?SERVER, stop).

%% Called when server process started (separate process). Kicks off Cowboy server.
init([Port]) ->
	{ok, Ip} = inet_parse:address(binary_to_list(ezchat_server:ezchat_domain())),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/ping", ping_handler, []},
			{"/echo", echo_handler, []},
			{"/ws/:room/:username", connection_handler, []},
			{"/[...]", cowboy_static, {priv_dir, ezchat, "",
				[{mimetypes, cow_mimetypes, all}]}}
		]}
	]),

	Options = [
		{ip, Ip}, {port, Port}
	],
	Env = [
		{env, [{dispatch, Dispatch}]},
		{onrequest, fun ezchat_utils:set_request_cors/1}
	],

%	{ok, _} = cowboy:start_http(http, 100, Options, Env),

	
	application:start(asn1),
	application:start(public_key),
	application:start(ssl),

	case ezchat_server:is_secure() of
		true ->
			erlang:display("Setting Websockets to SECURE"),
			cowboy:start_https(https, 100, [
				{ip, Ip}, {port, Port},
				{cacertfile, ezchat_server:secure_ca_cert()},
				{certfile, ezchat_server:secure_cert()},
				{keyfile, ezchat_server:secure_key()},
				{reuseaddr, true},
				{fail_if_no_peer_cert, true}
			], Env);
		_ ->
			erlang:display("Setting Websockets to NON-SECURE"),
			{ok, _} = cowboy:start_http(http, 100, Options, Env)
	end,
	
	log4erl:info("Cowboy server started~n"),
	%ok = ensure_contact(), % we need to ensure we have clustered before setting up the store.
	erlang:display("Connection store inited...~n"),
	connection_store:init(), % initialise Mnesia server for this node.
	erlang:display("Server running...~n"),
	{ok, []}.

%% Handles passed messages sychronously.
handle_call(_Request, _From, State) ->
	{noreply, State}.

%% Handles unexpected (not pre-defined, but not necessarily erroneous) passed messages.
handle_info(timeout, State) ->
	{noreply, State}.

%% Handles passed messages asychronously.
handle_cast(stop, State) ->
	{stop, normal, State}.

%% Default terminate function (hijack - actually handled by OTP)
terminate(_Reason, _State) ->
	ok.

%% Default code update function (hijack - actually handled by OTP)
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Forces current node to join the cluster if one exists. cluster
%% is denoted by a single node name passed at startup as 
%% env var 'cluster'.
ensure_contact() ->
	case cluster_node(cluster) of
		undefined ->
			ok;
		DefaultNodes ->
			ensure_contact(DefaultNodes)
	end.

ensure_contact(ContactNodes) ->
	Answering = [N || N <- ContactNodes, net_adm:ping(N) =:= pong],
	case Answering of
		[] ->
			{error, no_contact_nodes_reachable};
		_ ->
			DefaultTime = 6000,
			WaitTime = ezchat_utils:get_env(ezchat, wait_time, DefaultTime),
			wait_for_nodes(length(Answering), WaitTime)
	end.

%% Acts as a timer to allow for nodes to come online and join the
%% cluster. If no nodes join, then it is assumed this is the
%% only node.
wait_for_nodes(MinNodes, WaitTime) ->
	Slices = 10,
	SliceTime = round(WaitTime / Slices),
	wait_for_nodes(MinNodes, SliceTime, Slices).

wait_for_nodes(_MinNodes, _SliceTime, 0) ->
	ok;
wait_for_nodes(MinNodes, SliceTime, Iterations) ->
	case length(nodes()) > MinNodes of
		true ->
			ok;
		false ->
			timer:sleep(SliceTime),
			wait_for_nodes(MinNodes, SliceTime, Iterations - 1)
	end.

is_numeric(L) when is_list(L) ->
    Float = (catch erlang:list_to_float(L)),
    Int = (catch erlang:list_to_integer(L)),
    is_number(Float) orelse is_number(Int).