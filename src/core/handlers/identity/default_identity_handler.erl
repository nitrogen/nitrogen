% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_identity_handler).
-behaviour (identity_handler).
-export ([
	init/1, 
	finish/2,
	get_user/2, 
	set_user/3,
	logout/2
]).

init(Context) -> 
	{ok, Context, []}.

finish(Context, State) -> 
	{ok, Context, State}.

get_user(Context, State) -> 
	{ok, Context, State, undefined}.

set_user(Context, State, _User) -> 
	{ok, Context, State}.

logout(Context, State) -> 
	{ok, Context, State}.