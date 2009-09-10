% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_identity_handler).
-behaviour (identity_handler).
-export ([
	init/1, 
	finish/1,
	get_user/1, 
	set_user/2,
	clear/1
]).

init(State) -> 
	{ok, State}.

finish(State) -> 
	{ok, State}.

get_user(_State) -> 
	undefined.

set_user(_User, State) -> 
	{ok, State}.

clear(State) -> 
	{ok, State}.