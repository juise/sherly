-module(sherly_ws).

-behaviour(cowboy_websocket_handler).

%% Cowboy_websocket_handler callbacks
-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

%% API
-export([captured_output/1]).

-record(state, {
    bindings = [],
    accumulator = []
}).

%% ===================================================================
%% Cowboy_websocket_handler callbacks
%% ===================================================================

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    erlang:group_leader(whereis(sherly_io_server), self()),
    lager:info("Connection from ~s, ~s", [element(9, Req), proplists:get_value(<<"user-agent">>, element(17, Req))]),
    {ok, Req, #state{}}.

websocket_handle({text, <<>>}, Req, State) ->
    {ok, Req, State};

websocket_handle({text, <<"version">>}, Req, State) ->
    Resp = list_to_binary(sherly_eval:v()),
    {reply, {text, Resp}, Req, State};

websocket_handle({text, Msg}, Req, #state{bindings = Bindings, accumulator = Accumulator} = State) ->
    {Value, NewBindings, NewAccumulator} = sherly_eval:e(Msg, Bindings, Accumulator),
    case Value of
        [] when NewAccumulator =/= Accumulator ->
            {ok, Req, State#state{accumulator = NewAccumulator}};
        _ ->
            Result = captured_output([]) ++ Value,
            {reply, {text, list_to_binary(Result)}, Req, State#state{bindings = NewBindings}}
    end;

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

captured_output(Acc) ->
    receive
        {captured, Msg} ->
            ?MODULE:captured_output([Msg | Acc])
    after
        10 ->
            lists:reverse(Acc)
    end.
