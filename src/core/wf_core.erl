-module (wf_core).
-include ("wf.inc").
-include ("simplebridge.hrl").
-export ([
	run/1
]).

% nitrogen_core - 
% --
% Render a single Nitrogen page or inline application. This can be called
% from other Erlang web frameworks or resource servers, such as WebMachine, 
% Erlang Web, ErlyWeb, etc.

run(Context) ->
	?DEBUG,
	% Call init/2 on handlers.
	Context1 = init_handlers(Context),
	?DEBUG,

	% Route the request.
	Context2 = wf_route:route(Context1),
	?DEBUG,
	
	% Check if this is the first request.
	IsFirstRequest = is_first_request(Context2),
	?DEBUG,
	Context3 = case IsFirstRequest of
		true -> wf_core_firstrequest:run(Context2);			
		_    -> wf_core_postback:run(Context2)
	end,
	
	% Call finish/2 on handlers.
	?DEBUG,
	Context4 = finish_handlers(Context3),
	?DEBUG,
	
	% Send the response.
	ResponseBridge = Context4#context.response,
	?DEBUG,
	ResponseBridge:build_response().
	
	
% init_handlers/1 - 
% Handlers are initialized in the order that they exist in #context.handlers. The order
% is important, as some handlers may depend on others being initialize. For example, 
% the session handler may use the cookie handler to get or set the session cookie.
init_handlers(Context) ->
	?DEBUG,
	Handlers = Context#context.handlers,
	F = fun({HandlerName, Module, _State}, Cx) -> 
		?PRINT(HandlerName),
		?PRINT(Module),
		{ok, NewContext, NewState} = Module:init(Cx),
		?PRINT({ok, NewContext, NewState}),
		wf_context:set_handler(HandlerName, Module, NewState, NewContext)
	end,
	lists:foldl(F, Context, Handlers).

% finish_handlers/1 - 
% Handlers are finished in the order that they exist in #context.handlers. The order
% is important, as some handlers should finish after others. At the very least,
% the 'render' handler should go last to make sure that it captures all changes
% put in place by the other handlers.
finish_handlers(Context) ->
	Handlers = Context#context.handlers,
	F = fun({HandlerName, Module, State}, Cx) -> 
		{ok, NewContext, NewState} = Module:finish(Cx, State),
		wf_context:set_handler(HandlerName, Module, NewState, NewContext)
	end,
	lists:foldl(F, Context, Handlers).


% is_first_request/1 - 
% Return true if this is the first request.
is_first_request(_Context) -> true.