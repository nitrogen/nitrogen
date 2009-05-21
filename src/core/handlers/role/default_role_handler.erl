% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_role_handler).
-behaviour (role_handler).
-export ([
	init/1, 
	finish/2,
	get_has_role/3, 
	set_has_role/4, 
	get_roles/2,
	logout/2
]).

init(Context) -> 
	{ok, Context, []}.

finish(Context, State) -> 
	{ok, Context, State}.

get_has_role(Context, State, _Role) -> 
	{ok, Context, State, false}.

set_has_role(Context, State, _Role, _IsInRole) -> 
	{ok, Context, State}.

get_roles(Context, State) -> 
	{ok, Context, State, []}.

logout(Context, State) -> 
	{ok, Context, State}.