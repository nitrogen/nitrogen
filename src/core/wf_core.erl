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
	{ok, Context1} = deserialize_context(Context),
	?PRINT(Context1),
	
	% Initialize all handlers...
	{ok, Context2} = call_init_on_handlers(Context1),
	run_execute(Context2).
	 
run_execute(Context) ->
	% Deserialize the event if available...
	{ok, Context1} = wf_event:update_context_with_event(Context),

	% TODO - Check for access

	% Call the module...
	Event = Context1#context.event_context,
	IsFirstRequest = Event#event_context.is_first_request,
	{ok, Context2} = case IsFirstRequest of
		true  -> run_execute_first_request(Context1);
		false -> run_execute_postback(Context1)
	end,
	run_render(Context2).
		
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
	State = serialize_context(Context1),
	run_output(Html, [State, Javascript], Context1).
	
run_output(Html, Javascript, Context) ->
	% Output the results to the browser...
	output_handler:build_response(Html, Javascript, Context).



%%% SERIALIZE AND DESERIALIZE STATE %%%

% serialize_context_state/1 -
% Serialize part of Context and send it to the browser
% as Javascript variables.
serialize_context(Context) ->
	Page = Context#context.page_context,
	Handlers = Context#context.handler_list,
	SerializedContextState = wff:pickle([Page, Handlers]),
	[
		wf_render_actions:generate_scope_script(Context),
		wff:f("Nitrogen.$set_param('pageContext', '~s');", [SerializedContextState])
	].

% deserialize_context_state/1 -
% Updates the context with values that were stored
% in the browser by serialize_context_state/1.
deserialize_context(Context) ->	
	Request = Context#context.request,
	Params = Request:query_params(),
	
	% Deserialize page_context and handler_list if available...
	SerializedPageContext = proplists:get_value("pageContext", Params),
	[Page, Handlers] = case SerializedPageContext of
		undefined -> [Context#context.page_context, Context#context.handler_list];
		Other -> wff:depickle(Other)
	end,	
	
	% Deserialize dom_paths if available...
	DomPathList = proplists:get_value("domPaths", Params),
	DomPaths = wf_path:split_dom_paths(Page#page_context.name, DomPathList),
	
	% Create a new context...
	Context1 = Context#context { 
		page_context = Page, 
		handler_list=Handlers, 
		dom_paths=DomPaths 
	},

	% Return the new context...
	{ok, Context1}.
	
	
	
%%% SET UP AND TEAR DOWN HANDLERS %%%
	
% init_handlers/1 - 
% Handlers are initialized in the order that they exist in #context.handlers. The order
% is important, as some handlers may depend on others being initialize. For example, 
% the session handler may use the cookie handler to get or set the session cookie.
call_init_on_handlers(Context) ->
	Handlers = Context#context.handler_list,
	F = fun(#handler_context { name=Name, module=Module, state=State }, Cx) -> 
		{ok, NewContext, NewState} = Module:init(Cx, State),
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
	
	
	
%%% FIRST REQUEST %%%

run_execute_first_request(Context) ->
	% Some values...
	Event = Context#context.event_context,
	Module = Event#event_context.module,
	% Call Module:main/1
	{ok, Data, Context1} = call_module_main(Module, Context),
	{ok, Context1#context { data=Data}}.

call_module_main(Module, Context) ->
	?PRINT(Module),
	{module, Module} = code:ensure_loaded(Module),
	{ok, _Data, _NewContext} = wf_context:call_with_context(Module, main, [], Context, true).


%%% POSTBACK REQUEST %%%

run_execute_postback(Context) ->
	% Some values...
	Event = Context#context.event_context,
	Module = Event#event_context.module,
	Tag = Event#event_context.tag,
	
	% Validate...
	{ok, IsValid, Context1} = wf_validation:validate(Context),
	
	% Call the event...
	case IsValid of
		true -> call_module_event(Module, Tag, Context1);
		false -> {ok, Context1}
	end.
	
call_module_event(Module, Tag, Context) ->
	{ok, _NewContext} = wf_context:call_with_context(Module, event, [Tag], Context, false).