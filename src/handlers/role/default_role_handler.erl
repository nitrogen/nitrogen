% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_role_handler).
-behaviour (role_handler).
-export ([
	init/2, 
	finish/2,
	get_has_role/3, 
	set_has_role/4, 
	get_roles/2,
	clear_all/2
]).

init(_Config, State) -> 
	{ok, State}.

finish(_Config, State) -> 
	{ok, State}.

get_has_role(_Config, State, _Role) -> 
	{ok, false, State}.

set_has_role(_Role, _IsInRole, _Config, State) -> 
	{ok, State}.

get_roles(_Config, State) -> 
	{ok, [], State}.

clear_all(_Config, State) -> 
	{ok, State}.