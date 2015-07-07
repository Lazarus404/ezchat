%%%---------------------------------------------------------------------------
%%% @doc Starts RabbitMQ and handles receiving messages for local processes.
%%% @end
%%%---------------------------------------------------------------------------

-module(mq_server_receiver).
-author('Lee Sylvester <lee.sylvester@gmail.com>').

-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([
        start_link/0
    ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Callbacks

init([]) ->
    {ok, Connection} = amqp_connection:start(#amqp_params_network{ host="localhost" }),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    amqp_channel:call(Channel, #'exchange.declare'{exchange = <<"webrtc_comms">>,
                                                   type = <<"direct">>}),
    #'queue.declare_ok'{queue = Queue} =
        amqp_channel:call(Channel, #'queue.declare'{exclusive = true}),
    State = {Channel, Connection},
    amqp_channel:call(Channel, #'queue.bind'{exchange = <<"webrtc_comms">>,
                                              routing_key = term_to_binary(node(self())),
                                              queue = Queue}),
    amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue,
        no_ack = true}, self()),
    receive
        #'basic.consume_ok'{} -> ok
    end,
    {ok, State}.

handle_call(_Info, _From, State) ->
    {noreply, State}.

handle_cast(_Info, State) ->
    {noreply, State}.

handle_info({#'basic.deliver'{exchange=_Exchange, routing_key=_Key}, {amqp_msg, _Data, Packet}}, State) ->
    {Pid, Msg} = binary_to_term(Packet),
    Pid ! Msg,
    {noreply, State};
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