% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_log_handler).
-behaviour (log_handler).
-export ([
	init/1, 
	finish/1,
	info/2, 
	warning/2, 
	error/2
]).

init(State) -> 
	{ok, State}.
	
finish(State) -> 
	{ok, State}.

info(S, State) -> 
	error_logger:info_msg([S, "\n"]),
	{ok, State}.

warning(S, State) -> 
	error_logger:warning_msg([S, "\n"]),
	{ok, State}.

error(S, State) -> 
	error_logger:error_msg([S, "\n"]),
	{ok, State}.