%%%---------------------------------------------------------------------------
%%% @doc Starts RabbitMQ and handles dispatching messages to remote
%%%      processes.
%%% @end
%%%---------------------------------------------------------------------------

-module(mq_server_emitter).

-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([
        start_link/0,
        message_process/2
    ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

message_process(Pid, Msg) ->
    gen_server:cast(?SERVER, {message_node, {Pid, Msg}}).

%% Callbacks


init([]) ->
    {ok, Connection} = amqp_connection:start(#amqp_params_network{ host="localhost" }),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    amqp_channel:call(Channel, #'exchange.declare'{exchange = <<"webrtc_comms">>,
                                                   type = <<"direct">>}),
    State = {Channel, Connection},
    {ok, State}.

handle_call(_Info, _From, State) ->
    {noreply, State}.

%% Message needs to be sent to a process on a different node, so send
%% the message across AMQP to the processes node so it can
%% be delegated from there. 
handle_cast({message_node, {Pid, Msg}}, State) when node(Pid) =/= node() ->
    Node = term_to_binary(node(Pid)),
    {Channel, _} = State,
    amqp_channel:cast(Channel,
          #'basic.publish'{
            exchange = <<"webrtc_comms">>,
            routing_key = Node},
          #amqp_msg{payload = term_to_binary({Pid, Msg})}),
    {noreply, State};

%% Message needs to be sent to a process on this node, so skip
%% the AMQP method and send it straight to the process. 
handle_cast({message_node, {Pid, Msg}}, State) ->
    Pid ! Msg,
    {noreply, State}.

handle_info(ok = _Info, State) ->
    {noreply, State}.

%% Make sure to close the AMQP connection and channel when terminating.
terminate(_Reason, State) ->
    {Channel, Connection} = State,
    ok = amqp_channel:close(Channel),
    ok = amqp_connection:close(Connection),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.