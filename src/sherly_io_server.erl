-module(sherly_io_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({io_request, From, ReplyAs, Request}, State) ->
    {Reply, Text} = request(Request),
    reply(From, ReplyAs, Reply, Text),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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

