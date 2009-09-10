% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_cache_handler).
-behaviour (cache_handler).
-export ([
	init/1, 
	finish/1,
	get_cached/4,
	clear/2, 
	clear_all/1
]).

init(State) -> 
	{ok, State}.

finish(State) -> 
	{ok, State}.

get_cached(_Key, Function, _TTL, State) -> 
	{ok, Function(), State}.

clear(_Key, State) -> 
	{ok, State}.

clear_all(State) -> 
	{ok, State}.