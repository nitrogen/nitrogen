% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_security_handler).
-behaviour (security_handler).
-export ([
	init/1, 
	finish/1
]).

init(State) -> 
	{ok, State}.

finish(State) -> 
	{ok, State}.