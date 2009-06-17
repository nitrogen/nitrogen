% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (config_handler).
-export ([
	behaviour_info/1,
	get_config/2, get_config/3, set_config/3, set_config/2
]).

% get_config(Application, Module, Key, DefaultValue, Context, State) -> Value.
% Retrieve a configuration value.
get_config(Key, Context) ->
	_Value = get_config(Key, undefined, Context).

get_config(Key, DefaultValue, Context) -> 
	_Value = wf_context:call_handler_function_readonly(config_handler, get_config, [Key, DefaultValue], Context).

set_config(Key, Value, Context) ->
	{ok, _NewContext} = wf_context:call_handler_function(config_handler, set_config, [Key, Value], Context).
	
set_config(PropList, Context) ->
	{ok, _NewContext} = wf_context:call_handler_function(config_handler, set_config, [PropList], Context).

behaviour_info(callbacks) -> [
	{init, 2},      
	{finish, 2},
	{get_config, 4},
	{set_config, 4},
	{set_config, 3}
];

behaviour_info(_) -> undefined.