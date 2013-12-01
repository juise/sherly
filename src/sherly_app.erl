-module(sherly_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
                {"/",               cowboy_static, {priv_file,  sherly, "index.html"}},
                {"/static/[...]",   cowboy_static, {priv_dir,   sherly, "static"}},
                {"/websocket",      sherly_ws, []}
            ]}
    ]),
    [{ok, IP}, {ok, Port}] = [application:get_env(sherly, Env) || Env <- [ip, port]],
    {ok, Pid} = cowboy:start_http(http, 100, [{ip, IP}, {port, Port}], [{env, [{dispatch, Dispatch}]}]),
    sherly_sup:start_link().

stop(_State) ->
    ok.
