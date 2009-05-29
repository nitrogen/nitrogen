% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_cookie_handler).
-behaviour (cookie_handler).
-export ([
	init/1, 
	finish/2, 
	cookie/6
]).

init(Context) -> 
	{ok, Context, []}.

finish(Context, State) -> 
	{ok, Context, State}.

cookie(_Key, _Value, _Path, _MinutesToLive, Context, State) -> 
	{ok, Context, State}.