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
	try
		inner_run(Context)
	catch Type : Message -> 
		?LOG("~p~n", [{error, Type, Message, erlang:get_stacktrace()}])
	end.

inner_run(Context) ->
	% Call init/2 on handlers.
	{ok, Context1} = init_handlers(Context),

	% Route the request.
	{ok, Context2} = route_handler:route(Context1),

	% Process the request...
	IsFirstRequest = Context2#context.is_first_request,
	{ok, Context3} = case IsFirstRequest of
		true -> wf_core_firstrequest:run(Context2);
		_    -> wf_core_postback:run(Context2)
	end,
	
	% Render the request...
	Elements = Context3#context.data,
	Actions = Context3#context.queued_actions,
	Context4 = Context3#context { data=[], queued_actions=[] },
	{ok, Html, Javascript, Context5} = render_handler:render(Elements, Actions, Context4),
	
	% Call finish/2 on handlers.
	{ok, Context6} = finish_handlers(Context5),
	
	% Build and send the response.
	output_handler:build_response(Html, Javascript, Context6).
	
	
% init_handlers/1 - 
% Handlers are initialized in the order that they exist in #context.handlers. The order
% is important, as some handlers may depend on others being initialize. For example, 
% the session handler may use the cookie handler to get or set the session cookie.
init_handlers(Context) ->
	Handlers = Context#context.handlers,
	F = fun({HandlerName, Module, _State}, Cx) -> 
		{ok, NewContext, NewState} = Module:init(Cx),
		wf_context:set_handler(HandlerName, Module, NewState, NewContext)
	end,
	{ok, lists:foldl(F, Context, Handlers)}.

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
	{ok, lists:foldl(F, Context, Handlers)}.