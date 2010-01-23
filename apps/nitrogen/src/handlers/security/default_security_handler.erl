% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_security_handler).
-behaviour (security_handler).
-export ([
	init/2, 
	finish/2
]).

init(_Config, State) -> 
	{ok, State}.

finish(_Config, State) -> 
	{ok, State}.