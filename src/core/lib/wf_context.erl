-module (wf_context).
-include ("wf.inc").
-export ([
	make_context/2, 
	apply/3, apply/4,
	apply_return_raw/3, apply_return_raw/4,
	set_handler/4
]).

make_context(RequestBridge, ResponseBridge) ->
	% Create the new context using the default handlers.
	#context {
		request = RequestBridge,
		response = ResponseBridge,
		handlers = [
			{log, default_log_handler, []},
			{cookie, default_cookie_handler, []},
			{'query', default_query_handler, []},
			{session, default_session_handler, []},
			{identity, default_identity_handler, []},
			{role, default_role_handler, []},
			{route, default_route_handler, []},
			{event, default_event_handler, []},
			{security, default_security_handler, []},
			{cache, default_cache_handler, []},
			{state, default_state_handler, []},
			{render, default_render_handler, []},
			{output, page_output_handler, []}
		]
	}.
	

% apply/3 - 
% Helper function to call a function within a handler.
% Returns {ok, NewContext} or {ok, Value, NewContext}.
apply(HandlerName, FunctionName, Context) ->
	apply(HandlerName, FunctionName, [], Context).
	
% apply/4 - 
% Helper function to call a function within a handler.
% Returns {ok, NewContext} or {ok, Value, NewContext}.
apply(HandlerName, FunctionName, Args, Context) ->
	% Get the handler and state from the context. Then, call
	% the function, passing in the Args with Context and State prepended.
	{ok, Module, State} = get_handler(HandlerName, Context),
	Result = erlang:apply(Module, FunctionName, Args ++ [Context, State]),
	
	% Result will be {ok, Context, State} or {ok, Context, State, ReturnValue}.
	% Update the context with the new state.
	case Result of
		{ok, NewContext, NewState} -> 
			NewContext1 = set_handler(HandlerName, Module, NewState, NewContext),
			{ok, NewContext1};
			
		{ok, Value, NewContext, NewState} ->
			NewContext1 = set_handler(HandlerName, Module, NewState, NewContext),
			{ok, Value, NewContext1};

		{ok, Value1, Value2, NewContext, NewState} ->
			NewContext1 = set_handler(HandlerName, Module, NewState, NewContext),
			{ok, Value1, Value2, NewContext1}
	end.

apply_return_raw(HandlerName, FunctionName, Context) ->
	apply_return_raw(HandlerName, FunctionName, [], Context).
	
apply_return_raw(HandlerName, FunctionName, Args, Context) ->
	% Get the handler and state from the context. Then, call
	% the function, passing in the Args with Context and State prepended.
	{ok, Module, State} = get_handler(HandlerName, Context),
	erlang:apply(Module, FunctionName, Args ++ [Context, State]).

% get_handler/2 - 
% Look up a handler in a context. Return {ok, HandlerModule, State}
get_handler(HandlerName, Context) -> 
	case lists:keysearch(HandlerName, 1, Context#context.handlers) of
		{value, {HandlerName, Module,State}} -> 
			{ok, Module, State};
		false -> 
			throw({handlers_not_found_in_context, HandlerName, Context#context.handlers})
	end.
			
% set_handler/4 -
% Set a handler in a context. Returns the new context.
set_handler(HandlerName, Module, State, Context) ->
	Handlers = Context#context.handlers,
	NewHandlers = lists:keyreplace(HandlerName, 1, Handlers, {HandlerName, Module, State}),
	Context#context { handlers=NewHandlers }.
	
	
	
	