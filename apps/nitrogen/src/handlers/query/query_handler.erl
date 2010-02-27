% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (query_handler).
-export ([
    behaviour_info/1,
    get_value/1,
    get_values/1
]).


% get_value(Path, State) -> Value.  Given a path, return the parameter
% value, undefined, or throw an exception if there are too many
% matches.
get_value(Path) ->
    _Value = wf_handler:call_readonly(query_handler, get_value, [Path]).

% get_value(Path, State) -> [Values].
% Given a path, return a list of values.
get_values(Path) ->
    wf_handler:call_readonly(query_handler, get_values, [Path]).


behaviour_info(callbacks) -> [
    {init, 2},      
    {finish, 2},
    {get_value, 3},
    {get_values, 3}
];

behaviour_info(_) -> undefined.
