-module(sherly_io_server).

%% API
-export([start_link/0,
         init/0,
         loop/0]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    case whereis(?MODULE) of
        undefined ->
            try register(?MODULE, spawn_link(?MODULE, init, [])) of
                true -> {ok, whereis(?MODULE)}
            catch
                error:_Reason -> {error, already_started}
            end;
        Pid when is_pid(Pid) ->
            {error, already_started}
    end.

init() ->
    whereis(?MODULE) == self() andalso ?MODULE:loop().

loop() ->
    receive
        {io_request, From, ReplyAs, Request} ->
            {Reply, Text} = request(Request),
            reply(From, ReplyAs, Reply, Text);
        _Message ->
            nop
    after
        1000 ->
            ?MODULE:loop()
    end,
    ?MODULE:loop().

%% ===================================================================
%% Internal functions
%% ===================================================================

request({put_chars, Chars}) ->
    request({put_chars, latin1, Chars});

request({put_chars, Encoding, Chars}) ->
    Text = unicode:characters_to_list(Chars, Encoding),
    {ok, Text};

request({put_chars, Module, Function, Args}) ->
    request({put_chars, latin1, Module, Function, Args});

request({put_chars, Encoding, Module, Function, Args}) ->
    try
        request({put_chars, Encoding, apply(Module, Function, Args)})
    catch
        _Class:_Reason ->
            {error, Function}
    end;

request({requests, Reqs}) ->
     multi_request(Reqs, {ok, ok});

request(_Request) ->
    {error, enotsup}.


reply(From, ReplyAs, Reply, Text) ->
    From ! {io_reply, ReplyAs, Reply},
    From ! {captured, Text}.


multi_request([Req | Reqs], {ok, _Result}) ->
    multi_request(Reqs, request(Req));
multi_request([_ | _], Error) ->
    Error;
multi_request([], Result) ->
    Result.
