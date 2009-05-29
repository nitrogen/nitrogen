% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_session_handler).
-behaviour (session_handler).
-export ([
	init/1, 
	finish/2,
	get/4, 
	put/4, 
	clear/3, 
	clear_all/2
]).

init(Context) -> 
	{ok, Context, []}.

finish(Context, State) -> 
	{ok, Context, State}.
	
get(_Key, DefaultValue, Context, State) -> 
	{ok, DefaultValue, Context, State}.
	
put(_Key, _Value, Context, State) -> 
	{ok, Context, State}.
	
clear(_Key, Context, State) ->
	{ok, Context, State}.

clear_all(Context, State) -> 
	{ok, Context, State}.
