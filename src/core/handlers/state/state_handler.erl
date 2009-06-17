% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (state_handler).
-export ([
	behaviour_info/1,
	get_state/2, get_state/3, set_state/3, clear/2, clear_all/1
]).



% get_state(Key, DefaultValue, Context, State) -> Value.
% Retrieve a value from the storage area.
get_state(Key, Context) -> 
	_Value = get_state(Key, undefined, Context).
	
% get_state(Key, DefaultValue, Context, State) -> Value.
% Retrieve a value from the storage area.
get_state(Key, DefaultValue, Context) ->
	_Value = wf_context:call_handler_function_readonly(state_handler, get_state, [Key, DefaultValue], Context).

% set_state(Key, Value, Context, State) -> {ok, NewContext, NewState}.
% Put a value into the storage area.
set_state(Key, Value, Context) ->
	{ok, _NewContext} = wf_context:call_handler_function(state_handler, set_state, [Key, Value], Context).

% clear(Key, Context, State) -> {ok, NewContext, NewState}.
% Remove a value from the storage area.
clear(Key, Context) ->
	{ok, _NewContext} = wf_context:call_handler_function(state_handler, clear, [Key], Context).
	
% clear_all(Context, State) -> {ok, NewContext, NewState}.
% Clear all values from the storage area.
clear_all(Context) ->
	{ok, _NewContext} = wf_context:call_handler_function(state_handler, clear_all, Context).



behaviour_info(callbacks) -> [
	{init, 2},      
	{finish, 2},
	{get_state, 4},       
	{set_state, 4},	
	{clear, 3},
	{clear_all, 2}
];

behaviour_info(_) -> undefined.