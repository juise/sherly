-module(sherly_eval).

%% API
-export([e/3,
         v/0]).

-define(DOT, {dot, 1}).

%% ===================================================================
%% API functions
%% ===================================================================

e(Command, Bindings, Accumulator) ->
    try
        evaluate(tokens(erlang:binary_to_list(Command), Accumulator), Bindings)
    catch
        error:{unbound_var, Var} ->
            {"variable '" ++ atom_to_list(Var) ++ "' is unbound", Bindings, []};
        error:{disabled_fun, Fun} ->
            {"function '" ++ atom_to_list(Fun) ++ "' is disabled", Bindings, []};
        Class:Reason ->
            {io_lib:format("~p", [{Class, Reason}]), Bindings, []}
    end.

v() ->
    erlang:system_info(system_version).

%% ===================================================================
%% Internal functions
%% ===================================================================

tokens(Command, Accumulator) ->
    {ok, Tokens, _} = erl_scan:string(Command),
    case lists:reverse(Tokens) of
        [?DOT | _] ->
            {ok, Accumulator ++ Tokens};
        _ ->
            {acc, Accumulator ++ Tokens}
    end.

evaluate({acc, Tokens}, Bindings) ->
    {[], Bindings, Tokens};
evaluate({ok, Tokens}, Bindings) ->
    Exprs = parse(Tokens),
    {value, Result, NewBindings} = erl_eval:exprs(Exprs, Bindings),
    {io_lib:format("~p", [Result]), NewBindings, []}.

parse(Tokens) ->
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    lists:foldl(fun filter/2, Exprs, Exprs).


filter({_, _, {_, _, {atom, _, init}, {atom, _, Fun}}, _}, _) -> error({disabled_fun, Fun});
filter({_, _, {_, _, {atom, _, erlang}, {atom, _, halt}}, _}, _) -> error({disabled_fun, halt});
filter({_, _, {_, _, {atom, _, shell_default}, {atom, _, q}}, _}, _) -> error({disabled_fun, q});
filter(_, Exprs) -> Exprs.

