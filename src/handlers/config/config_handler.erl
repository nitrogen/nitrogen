% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (config_handler).
-export ([
	behaviour_info/1,
	get_config/1, get_config/2, set_config/2, set_config/1
]).

% get_config(Application, Module, Key, DefaultValue, State) -> Value.
% Retrieve a configuration value.
get_config(Key) ->
	_Value = get_config(Key, undefined).

get_config(Key, DefaultValue) -> 
	_Value = wf_handler:call(config_handler, get_config, [Key, DefaultValue]).

set_config(Key, Value) ->
	ok = wf_handler:call(config_handler, set_config, [Key, Value]).
	
set_config(PropList) ->
	ok = wf_handler:call(config_handler, set_config, [PropList]).

behaviour_info(callbacks) -> [
	{init, 1},      
	{finish, 1},
	{get_config, 3},
	{set_config, 3},
	{set_config, 2}
];

behaviour_info(_) -> undefined.