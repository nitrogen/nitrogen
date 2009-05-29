% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_log_handler).
-behaviour (log_handler).
-export ([
	init/1, 
	finish/2,
	info/3, 
	warning/3, 
	error/3
]).

init(Context) -> 
	{ok, Context, []}.
	
finish(Context, State) -> 
	{ok, Context, State}.

info(S, Context, State) -> 
	error_logger:info_msg([S, "\n"]),
	{ok, Context, State}.

warning(S, Context, State) -> 
	error_logger:warning_msg([S, "\n"]),
	{ok, Context, State}.

error(S, Context, State) -> 
	error_logger:error_msg([S, "\n"]),
	{ok, Context, State}.