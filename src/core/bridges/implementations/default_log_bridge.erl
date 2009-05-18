% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_log_bridge).
-behaviour (log_bridge).
-export ([
	init/1, render/1,
	info/1, warning/1, error/1
]).

init(Context) -> Context.
render(Context) -> Context.

info(S) -> error_logger:info_msg(S).

warning(S) -> error_logger:warning_msg(S).

error(S) -> error_logger:error_msg(S).