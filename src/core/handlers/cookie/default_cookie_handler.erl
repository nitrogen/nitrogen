% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_cookie_handler).
-behaviour (cookie_handler).
-include ("wf.inc").
-export ([
	init/2, 
	finish/2,
	get_cookie/3,
	set_cookie/6
]).

init(Context, State) -> 
	{ok, Context, State}.
	
finish(Context, State) -> 
	{ok, Context, State}.

get_cookie(_Key, _Context, _State) -> 
	undefined.
	
set_cookie(_Key, _Value, _Path, _MinutesToLive, Context, State) -> 
	{ok, Context, State}.