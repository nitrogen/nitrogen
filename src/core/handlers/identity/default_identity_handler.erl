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

init(Context, State) -> 
	{ok, Context, State}.

finish(Context, State) -> 
	{ok, Context, State}.

get_user(_Context, _State) -> 
	undefined.

set_user(_User, Context, State) -> 
	{ok, Context, State}.

clear(Context, State) -> 
	{ok, Context, State}.