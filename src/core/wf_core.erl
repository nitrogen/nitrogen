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
	run_execute(Context2).
	 
run_execute(Context) ->
	% Check the event...
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
	State = serialize_context_state(Context1),
	run_output(Html, [State, Javascript], Context1).
	
run_output(Html, Javascript, Context) ->
	% Output the results to the browser...
	output_handler:build_response(Html, Javascript, Context).



%%% SERIALIZE AND DESERIALIZE STATE %%%

% serialize_context_state/1 -
% Serialize part of Context and send it to the browser
% as Javascript variables.
serialize_context_state(Context) ->
	Page = Context#context.page_context,
	Handlers = Context#context.handler_list,
	SerializedContextState = wff:pickle([Page, Handlers]),
	[
		wf_render_actions:generate_scope_script(Context),
		wff:f("Nitrogen.$set_param('contextState', '~s');", [SerializedContextState])
	].

% deserialize_context_state/1 -
% Updates the context with values that were stored
% in the browser by serialize_context_state/1.
deserialize_context_state(Context) ->	
	% Get the contextState parameter.
	Request = Context#context.request,
	Params = Request:query_params(),
	SerializedContextState = proplists:get_value("contextState", Params),
	
	% Deserialize if possible, using the existing state as default.
	[Page, Handlers] = case SerializedContextState of
		undefined -> [Context#context.page_context, Context#context.handler_list];
		Other -> wff:depickle(Other)
	end,
	
	% Update the context and return.
	{ok, Context#context { page_context = Page, handler_list=Handlers }}.
	
	
	
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
	{module, Module} = code:ensure_loaded(Module),
	case erlang:function_exported(Module, main, 1) of 
		true -> call_module_main_with_context(Module, Context);
		false -> call_module_main_without_context(Module, Context)
	end.
	
call_module_main_with_context(Module, Context) ->		
	{ok, _Data, _Context1} = Module:main(Context).

call_module_main_without_context(Module, Context) ->
	put(context, Context),
	Data = Module:main(),
	Context1 = get(context),
	{ok, Data, Context1}.



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
	case erlang:function_exported(Module, event, 2) of
		true -> call_module_event_with_context(Module, Tag, Context);
		false -> call_module_event_without_context(Module, Tag, Context)
	end.
	
call_module_event_with_context(Module, Tag, Context) ->
	{ok, _Context1} = Module:event(Tag, Context).
	
call_module_event_without_context(Module, Tag, Context) ->
	put(context, Context),
	Module:event(Tag),
	Context1 = get(context),
	{ok, Context1}.

