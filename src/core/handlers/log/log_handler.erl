% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (log_handler).
-export ([
	behaviour_info/1
]).

behaviour_info(callbacks) -> [
	% init(Context) -> {ok, NewContext, State}.
	% Called at the start of the request.
	{init, 1},      

	% finish(Context, State) -> {ok, NewContext, NewState}.
	% Called at the end of the request, before sending
	% a response back to the browser.
	{finish, 2},
	
	% info(Context, State, String) -> {ok, NewContext, NewState}.
	% Log an info-level message. Everything is functioning as usual.
	{info, 3},       
	
	% warning(Context, State, String) -> {ok, NewContext, NewState}.
	% Log a warning-level message. If something is not corrected, then
	% service could be interrupted in some way.
	{warning, 3},
	
	% error(Context, State, Key) -> {ok, NewContext, NewState}.
	% Log an error-level message. Service has been interrupted in some way.
	{error, 3}
];

behaviour_info(_) -> undefined.
