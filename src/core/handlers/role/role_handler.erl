% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (role_handler).
-export ([
	behaviour_info/1
]).

behaviour_info(callbacks) -> [
	% init(Context) -> {ok, NewContext, NewState}.
	% Called at the start of the request.
	{init, 1},      

	% finish(Context, State) -> {ok, NewContext, NewState}.
	% Called at the end of the request, before sending
	% a response back to the browser.
	{finish, 2},
	
	% role(Context, State, Role) -> {ok, NewContext, NewState, Boolean}.
	% Returns true or false depending on whether the user is in the specified role.
	{get_has_role, 3},
	
	% role(Context, State, Role, IsInRole) -> {ok, NewContext, NewState}.
	% Set whether the user is in the specified role.
	{set_has_role, 4},
	
	% roles(Context, State) -> {ok, NewContext, NewState, [Roles]}
	% Return a list of roles held by the current user
	{get_roles, 2},
	
	% logout(Context, State) -> {ok, NewContext, NewState}.
	% Clear all roles.
	{logout, 2}	
];

behaviour_info(_) -> undefined.
