-module (wf_handler).
-include ("wf.inc").
-export ([
	call/2, 
	call/3,
	call_readonly/2, 
	call_readonly/3,
	get_handler/1,
	set_handler/2, 
	set_handler/3
]).


% Helper function to call a function within a handler.
% Returns ok or {ok, Value}.
call(Name, FunctionName) -> call(Name, FunctionName, []).
	
% Helper function to call a function within a handler.
% Returns ok or {ok, Value}.
call(Name, FunctionName, Args) ->
	% Get the handler and state from the context. Then, call
	% the function, passing in the Args and State.
 	#handler_context { module=Module, state=State } = get_handler(Name),
	Result = erlang:apply(Module, FunctionName, Args ++ [State]),
	
	% Result will be {ok, State}, {ok, Value1, State}, or {ok, Value1, Value2, State}.
	% Update the context with the new state.
	case Result of
		{ok, NewState} -> 
			set_handler(Name, Module, NewState),
			ok;
			
		{ok, Value, NewState} ->
			set_handler(Name, Module, NewState),
			{ok, Value};

		{ok, Value1, Value2, NewState} ->
			set_handler(Name, Module, NewState),
			{ok, Value1, Value2}
	end.

call_readonly(Name, FunctionName) -> call_readonly(Name, FunctionName, []).
	
call_readonly(Name, FunctionName, Args) ->
	% Get the handler and state from the context. Then, call
	% the function, passing in the Args with State appended.
	#handler_context { module=Module, state=State } = get_handler(Name),
	erlang:apply(Module, FunctionName, Args ++ [State]).

% get_handler/2 - 
% Look up a handler in a context. Return {ok, HandlerModule, State}
get_handler(Name) -> 
	Context = wf_context:context(),
	case lists:keysearch(Name, 2, Context#context.handler_list) of
		{value, Handler} when is_record(Handler, handler_context) -> 
			Handler;
		false -> 
			throw({handler_not_found_in_context, Name, Context#context.handler_list})
	end.
			
% set_handler/2 -
% Set a handler in a context.
set_handler(Module, State) ->
	{module, Module} = code:ensure_loaded(Module),
	
	% Get the module's behavior...
	L = Module:module_info(attributes),
	Name = case proplists:get_value(behaviour, L) of
		[N] -> N;
		_      -> throw({must_define_a_nitrogen_behaviour, Module})
	end,
	set_handler(Name, Module, State).
		
set_handler(Name, Module, State) ->
	{module, Module} = code:ensure_loaded(Module),
	Context = wf_context:context(),
	Handlers = Context#context.handler_list,
	NewHandler = #handler_context { name=Name, module=Module, state=State },
	NewHandlers = lists:keyreplace(Name, 2, Handlers, NewHandler),
	wf_context:context(Context#context { handler_list=NewHandlers }).