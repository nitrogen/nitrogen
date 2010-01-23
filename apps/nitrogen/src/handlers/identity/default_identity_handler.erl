% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_identity_handler).
-behaviour (identity_handler).
-export ([
	init/2, 
	finish/2,
	get_user/2, 
	set_user/3,
	clear/2
]).

init(_Config, State) -> 
	{ok, State}.

finish(_Config, State) -> 
	{ok, State}.

get_user(_Config, _State) -> 
	undefined.

set_user(_User, _Config, State) -> 
	{ok, State}.

clear(_Config, State) -> 
	{ok, State}.