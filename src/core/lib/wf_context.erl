-module (wf_context).
-include ("wf.inc").
-export ([
	make_context/2, 
	apply/3, apply/4,
	apply_return_raw/3, apply_return_raw/4,
	set_handler/3, set_handler/4
]).

make_context(RequestBridge, ResponseBridge) ->
	% Create the new context using the default handlers.
	#context {
		request = RequestBridge,
		response = ResponseBridge,
		page_context = #page_context { series_id = wff:temp_id() },
		event_context = #event_context {},
		handler_list = [
			% Core handlers...
			make_handler(log_handler, default_log_handler),
			make_handler(process_cabinet_handler, default_process_cabinet_handler),
			make_handler(cache_handler, default_cache_handler), 
			make_handler(query_handler, default_query_handler),
			make_handler(cookie_handler, default_cookie_handler),
			
			% Stateful handlers...
			make_handler(session_handler, simple_session_handler), 
			make_handler(state_handler, default_state_handler), 
			make_handler(identity_handler, default_identity_handler), 
			make_handler(role_handler, default_role_handler), 
			
			% Handlers that possibly redirect...
			make_handler(route_handler, default_route_handler), 
			make_handler(security_handler, default_security_handler), 
			
			% Output handler...
			make_handler(output_handler, page_output_handler)
		]
	}.
	
make_handler(Name, Module) -> 
	#handler_context { 
		name=Name,
		module=Module,
		state=[]
	}.
	
% apply/3 - 
% Helper function to call a function within a handler.
% Returns {ok, NewContext} or {ok, Value, NewContext}.
apply(Name, FunctionName, Context) ->
	apply(Name, FunctionName, [], Context).
	
% apply/4 - 
% Helper function to call a function within a handler.
% Returns {ok, NewContext} or {ok, Value, NewContext}.
apply(Name, FunctionName, Args, Context) ->
	% Get the handler and state from the context. Then, call
	% the function, passing in the Args with Context and State prepended.
 	#handler_context { module=Module, state=State } = get_handler(Name, Context),
	Result = erlang:apply(Module, FunctionName, Args ++ [Context, State]),
	
	% Result will be {ok, Context, State} or {ok, Context, State, ReturnValue}.
	% Update the context with the new state.
	case Result of
		{ok, NewContext, NewState} -> 
			NewContext1 = set_handler(Name, Module, NewState, NewContext),
			{ok, NewContext1};
			
		{ok, Value, NewContext, NewState} ->
			NewContext1 = set_handler(Name, Module, NewState, NewContext),
			{ok, Value, NewContext1};

		{ok, Value1, Value2, NewContext, NewState} ->
			NewContext1 = set_handler(Name, Module, NewState, NewContext),
			{ok, Value1, Value2, NewContext1}
	end.

apply_return_raw(Name, FunctionName, Context) ->
	apply_return_raw(Name, FunctionName, [], Context).
	
apply_return_raw(Name, FunctionName, Args, Context) ->
	% Get the handler and state from the context. Then, call
	% the function, passing in the Args with Context and State prepended.
	#handler_context { module=Module, state=State } = get_handler(Name, Context),
	erlang:apply(Module, FunctionName, Args ++ [Context, State]).

% get_handler/2 - 
% Look up a handler in a context. Return {ok, HandlerModule, State}
get_handler(Name, Context) -> 
	case lists:keysearch(Name, 2, Context#context.handler_list) of
		{value, Handler} when is_record(Handler, handler_context) -> 
			Handler;
		false -> 
			throw({handler_not_found_in_context, Name, Context#context.handler_list})
	end.
			
% set_handler/4 -
% Set a handler in a context. Returns the new context.
set_handler(Module, State, Context) ->
	{module, Module} = code:ensure_loaded(Module),
	
	% Get the module's behavior...
	L = Module:module_info(attributes),
	Name = case proplists:get_value(behaviour, L) of
		[N] -> N;
		_      -> throw({must_define_a_nitrogen_behaviour, Module})
	end,
	set_handler(Name, Module, State, Context).
		
set_handler(Name, Module, State, Context) ->
	{module, Module} = code:ensure_loaded(Module),
	Handlers = Context#context.handler_list,
	NewHandler = #handler_context { name=Name, module=Module, state=State },
	NewHandlers = lists:keyreplace(Name, 2, Handlers, NewHandler),
	Context#context { handler_list=NewHandlers }.
	
	
	
	