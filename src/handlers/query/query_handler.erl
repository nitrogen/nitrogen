% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (query_handler).
-export ([
	behaviour_info/1,
	get_value/1
]).


% get_value(Path, State) -> Value.
% Given a path, return the parameter value.
get_value(Path) ->
	_Value = wf_handler:call_readonly(query_handler, get_value, [Path]).
		

behaviour_info(callbacks) -> [
	{init, 1},      
	{finish, 1},
	{get_value, 2}
];

behaviour_info(_) -> undefined.