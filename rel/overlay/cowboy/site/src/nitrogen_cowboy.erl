-module(nitrogen_cowboy).
-behaviour(cowboy_http_handler).
-export ([loop/1]).
-include_lib("nitrogen_core/include/wf.hrl").

-export([init/3, handle/2, terminate/2]).

-record(state, {headers, body}).

init({_Transport, http}, Req, Opts) ->
	Headers = proplists:get_value(headers, Opts, []),
	Body = proplists:get_value(body, Opts, "http_handler"),
	{ok, Req, #state{headers=Headers, body=Body}}.

handle(Req, State=#state{headers=Headers, body=Body}) ->
    {ok, DocRoot} = application:get_env(cowboy, document_root),
    RequestBridge = simple_bridge:make_request(cowboy_request_bridge,
                                               {Req, DocRoot}),
    ResponseBridge = simple_bridge:make_response(cowboy_response_bridge,
                                                 {Req, DocRoot}),
    nitrogen:init_request(RequestBridge, ResponseBridge),
    nitrogen:run().

terminate(_Req, _State) ->
	ok.
