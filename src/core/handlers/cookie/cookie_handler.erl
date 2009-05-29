% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (cookie_handler).
-export ([
	behaviour_info/1
]).

behaviour_info(callbacks) -> [
	% init(Context) -> {ok, NewContext, NewState}
	% Called at the start of the request.
	{init, 1},      

	% finish(Context, State) -> {ok, NewContext, NewState}
	% Called at the end of the request, before sending
	% a response back to the browser.
	{finish, 2},
	
	% cookie(Key, Value, Path, MinutesToLive, Context, State) -> {ok, NewContext, NewState}
	% Set the specified cookie.
	{cookie, 6}
];

behaviour_info(_) -> undefined.
