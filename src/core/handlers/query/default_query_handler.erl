% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_query_handler).
-behaviour (query_handler).
-export ([
	init/1, 
	finish/2,
	get_value/3
]).

init(Context) -> 
	{ok, Context, []}.
	
finish(Context, State) -> 
	{ok, Context, State}.

get_value(_Key, Context, State) ->
	{ok, undefined, Context, State}.