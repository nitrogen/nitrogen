% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (role_handler).
-export ([
	behaviour_info/1, get_has_role/2, set_has_role/3, get_roles/1, clear_all/1
]).



% get_has_role(Role, Context, State) -> {ok, IsInRole, NewContext, NewState}.
% Returns true or false depending on whether the user is in the specified role.
get_has_role(Role, Context) ->
	_Boolean = wf_context:call_handler_function_readonly(role_handler, get_has_role, [Role], Context).

% set_has_role(Role, IsInRole, Context, State) -> {ok, NewContext, NewState}.
% Set whether the user is in the specified role.
set_has_role(Role, IsInRole, Context) ->
	{ok, _NewContext} = wf_context:call_handler_function(role_handler, set_has_role, [Role, IsInRole], Context).
	
% roles(Context, State) -> {ok, [Roles], NewContext, NewState}
% Return a list of roles held by the current user
get_roles(Context) ->
	_Roles = wf_context:call_handler_function_readonly(role_handler, roles, Context).
	
% clear_all(Context, State) -> {ok, NewContext, NewState}.
% Clear all roles.
clear_all(Context) ->
	{ok, _NewContext} = wf_context:call_handler_function(role_handler, clear_all, Context).



behaviour_info(callbacks) -> [
	{init, 2},      
	{finish, 2},
	{get_has_role, 3},
	{set_has_role, 4},
	{get_roles, 2},
	{clear_all, 2}	
];
behaviour_info(_) -> undefined.