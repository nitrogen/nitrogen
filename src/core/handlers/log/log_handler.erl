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



% info(String, Context, State) -> {ok, NewContext, NewState}.
% Log an info-level message. Everything is functioning as usual.
info(String, Args, Context) -> 
	{ok, _NewContext} = info(wff:f(String, Args), Context).
	
info(String, Context) -> 
	{ok, _NewContext} = wf_context:apply(log_handler, info, [String], Context).
	
% warning(String, Context, State) -> {ok, NewContext, NewState}.
% Log a warning-level message. If something is not corrected, then
% service could be interrupted in some way.
warning(String, Args, Context) -> 
	{ok, _NewContext} = warning(wff:f(String, Args), Context).
	
warning(String, Context) -> 
	{ok, _NewContext} = wf_context:apply(log_handler, warning, [String], Context).

% error(String, Context, State) -> {ok, NewContext, NewState}.
% Log an error-level message. Service has been interrupted in some way.
error(String, Args, Context) -> 
	{ok, _NewContext} = error(wff:f(String, Args), Context).
	
error(String, Context) -> 
	{ok, _NewContext} = wf_context:apply(log_handler, error, [String], Context).



behaviour_info(callbacks) -> [
	{init, 2},      
	{finish, 2},
	{info, 3},       
	{warning, 3},	
	{error, 3}
];
behaviour_info(_) -> undefined.