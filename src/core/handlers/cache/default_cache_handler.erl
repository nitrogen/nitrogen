% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_cache_handler).
-behaviour (cache_handler).
-export ([
	init/1, 
	finish/2,
	get_set/5,
	clear/3, 
	clear_all/2
]).

init(Context) -> 
	{ok, Context, []}.

finish(Context, State) -> 
	{ok, Context, State}.

get_set(Context, State, _Key, Function, _TTL) -> 
	{ok, Context, State, Function()}.

clear(Context, State, _Key) -> 
	{ok, Context, State}.

clear_all(Context, State) -> 
	{ok, Context, State}.