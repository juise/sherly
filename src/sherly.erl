-module(sherly).

%% API
-export([start/0,
         stop/0]).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
    [application:start(Application) || Application <- [ranch, cowlib, cowboy]],
    application:start(sherly),
    ok.

stop() ->
    application:stop(sherly),
    [application:stop(Application) || Application <- [cowboy, cowlib, ranch]],
    ok.
