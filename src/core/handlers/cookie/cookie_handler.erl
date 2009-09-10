% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (cookie_handler).
-export ([
	behaviour_info/1,
	get_cookie/1,
	set_cookie/4
]).



% get_cookie(Key, State) -> Value.
% Read a cookie from the browser.
get_cookie(Key) -> 
	_Value = wf_handler:call_readonly(cookie_handler, get_cookie, [Key]).
	
% set_cookie(Key, Value, Path, MinutesToLive, State) -> {ok, NewState}.
% Send a cookie to the browser.
set_cookie(Key, Value, Path, MinutesToLive) -> 
	ok = wf_handler:call(cookie_handler, set_cookie, [Key, Value, Path, MinutesToLive]).

behaviour_info(callbacks) -> [
	{init, 1},      
	{finish, 1},
	{get_cookie, 2},
	{set_cookie, 5}
];

behaviour_info(_) -> undefined.
