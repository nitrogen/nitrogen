% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (log_handler).
-export ([
	behaviour_info/1,
	info/2, info/3,
	warning/2, warning/3,
	error/2, error/3
]).

behaviour_info(callbacks) -> [
	% init(Context) -> {ok, NewContext, State}.
	% Called at the start of the request.
	{init, 1},      

	% finish(Context, State) -> {ok, NewContext, NewState}.
	% Called at the end of the request, before sending
	% a response back to the browser.
	{finish, 2},
	
	% info(String, Context, State) -> {ok, NewContext, NewState}.
	% Log an info-level message. Everything is functioning as usual.
	{info, 3},       
	
	% warning(String, Context, State) -> {ok, NewContext, NewState}.
	% Log a warning-level message. If something is not corrected, then
	% service could be interrupted in some way.
	{warning, 3},
	
	% error(String, Context, State) -> {ok, NewContext, NewState}.
	% Log an error-level message. Service has been interrupted in some way.
	{error, 3}
];

behaviour_info(_) -> undefined.

info(String, Args, Context) -> info(wff:f(String, Args), Context).
info(String, Context) -> wf_context:apply(log, info, [String], Context).
	
warning(String, Args, Context) -> warning(wff:f(String, Args), Context).
warning(String, Context) -> wf_context:apply(log, warning, [String], Context).

error(String, Args, Context) -> error(wff:f(String, Args), Context).
error(String, Context) -> wf_context:apply(log, error, [String], Context).