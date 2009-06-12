% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_session_handler).
-behaviour (session_handler).
-export ([
	init/2, 
	finish/2,
	get_value/4, 
	set_value/4, 
	clear_value/3, 
	clear_all/2
]).

init(Context, State) -> 
	{ok, Context, State}.

finish(Context, State) -> 
	{ok, Context, State}.
	
get_value(_Key, DefaultValue, Context, State) -> 
	{ok, DefaultValue, Context, State}.
	
set_value(_Key, _Value, Context, State) -> 
	{ok, Context, State}.
	
clear_value(_Key, Context, State) ->
	{ok, Context, State}.

clear_all(Context, State) -> 
	{ok, Context, State}.
