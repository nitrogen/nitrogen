% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (security_bridge).
-export ([
	behaviour_info/1
]).

behaviour_info(callbacks) -> [
	% init(Context) -> ParameterizedModule
	% Called at the start of the request.
	{init, 1},      

	% render(Context) -> NewContext
	% Called at the end of the request, before sending
	% a response back to the browser.
	{render, 1},
	
	% user() -> User
	% Retrieve an Erlang term representing the current user.
	{user, 0},
	
	% user(Term) -> ParameterizedModule
	% Set an Erlang term representing the current user.
	{user, 1},
	
	% role(Role) -> true | false
	% Returns true or false depending on whether the user is in the specified role.
	{role, 1},
	
	% role(Role, IsInRole) -> ParameterizedModule
	% Set whether the user is in the specified role.
	{role, 2},
	
	% roles() -> [Roles]
	% Return a list of roles held by the current user
	{roles, 0},
	
	% logout() -> ParameterizedModule
	% Set the user to undefined, and clear all roles.
	{logout, 0}	
];

behaviour_info(_) -> undefined.
