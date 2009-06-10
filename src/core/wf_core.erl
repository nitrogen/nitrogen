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
		run_bootstrap(Context)
	catch Type : Message -> 
		?LOG("~p~n", [{error, Type, Message, erlang:get_stacktrace()}])
	end.

run_bootstrap(Context) ->
	% Get the handlers from querystring, if they exist...
	{ok, Context1} = deserialize_context_state(Context),
	
	% Initialize all handlers...
	{ok, Context2} = call_init_on_handlers(Context1),
	run_main(Context2).
	 
run_main(Context) ->
	% Route the request...
	{ok, Context1} = route_handler:route(Context),
	
	% Check the event...
	{ok, Context2} = wf_event:update_context_with_event(Context1),
 
	% Get event information...
	Event = Context2#context.event_context,
	IsFirstRequest = Event#event_context.is_first_request,
	Module = Event#event_context.module,
	Tag = Event#event_context.tag,

	% Call the module...
	{ok, Context4} = case IsFirstRequest of
		true -> 
			{ok, Data, Context3} = Module:main(Context2),
			{ok, Context3#context { data=Data}};

		_    -> 
			Module:event(Tag, Context2)
	end,
	run_render(Context4).
	
run_render(Context) ->
	Elements = Context#context.data,
	Actions = Context#context.queued_actions,
	Context1 = Context#context { data=[], queued_actions=[] },
	{ok, Html, Javascript, Context2} = wf_render:render(Elements, Actions, Context1),
	run_finish(Html, Javascript, Context2).

run_finish(Html, Javascript, Context) ->
	% Call finish on all handlers.
	{ok, Context1} = call_finish_on_handlers(Context),
	
	% Create Javascript to set the state...
	State = serialize_context_state(Context1),
	run_output(Html, [State, Javascript], Context1).
	
run_output(Html, Javascript, Context) ->
	% Output the results to the browser...
	output_handler:build_response(Html, Javascript, Context).

% deserialize_context_state/1 -
% Updates the context with values that were stored
% in the browser by serialize_context_state/1.
deserialize_context_state(Context) ->	
	% Quick and dirty function to read a query parameter.
	Request = Context#context.request,
	Params = Request:query_params(),
	GetParam = fun(Param, Default) ->
		case proplists:get_value(Param, Params) of
			undefined -> Default;
			Other -> wff:depickle(Other)
		end		
	end,

	% Get the pageContext and handlerList if they exist, and
	% stuff them into the context...
	Page = GetParam("pageContext", Context#context.page_context),
	Handlers = GetParam("handlerList", Context#context.handler_list),
	{ok, Context#context { page_context = Page, handler_list=Handlers }}.
	
% serialize_context_state/1 -
% Serialize part of Context and send it to the browser
% as Javascript variables.
serialize_context_state(Context) ->
	Page = Context#context.page_context,
	SerializedPage = wff:pickle(Page),
	Handlers = Context#context.handler_list,
	SerializedHandlers = wff:pickle(Handlers),
	[
		wf_render_actions:generate_scope_script(Context),
		wff:f("Nitrogen.$set_param('pageContext', '~s');\r\n", [SerializedPage]),
		wff:f("Nitrogen.$set_param('handlerList', '~s');\r\n", [SerializedHandlers])
	].
	
	
% init_handlers/1 - 
% Handlers are initialized in the order that they exist in #context.handlers. The order
% is important, as some handlers may depend on others being initialize. For example, 
% the session handler may use the cookie handler to get or set the session cookie.
call_init_on_handlers(Context) ->
	Handlers = Context#context.handler_list,
	F = fun(#handler_context { name=Name, module=Module }, Cx) -> 
		{ok, NewContext, NewState} = Module:init(Cx),
		wf_context:set_handler(Name, Module, NewState, NewContext)
	end,
	{ok, lists:foldl(F, Context, Handlers)}.

% finish_handlers/1 - 
% Handlers are finished in the order that they exist in #context.handlers. The order
% is important, as some handlers should finish after others. At the very least,
% the 'render' handler should go last to make sure that it captures all changes
% put in place by the other handlers.
call_finish_on_handlers(Context) ->
	Handlers = Context#context.handler_list,
	F = fun(#handler_context { name=Name, module=Module, state=State }, Cx) -> 
		{ok, NewContext, NewState} = Module:finish(Cx, State),
		wf_context:set_handler(Name, Module, NewState, NewContext)
	end,
	{ok, lists:foldl(F, Context, Handlers)}.