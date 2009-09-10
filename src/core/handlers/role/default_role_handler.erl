% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_role_handler).
-behaviour (role_handler).
-export ([
	init/1, 
	finish/1,
	get_has_role/2, 
	set_has_role/3, 
	get_roles/1,
	clear_all/1
]).

init(State) -> 
	{ok, State}.

finish(State) -> 
	{ok, State}.

get_has_role(State, _Role) -> 
	{ok, false, State}.

set_has_role(_Role, _IsInRole, State) -> 
	{ok, State}.

get_roles(State) -> 
	{ok, [], State}.

clear_all(State) -> 
	{ok, State}.