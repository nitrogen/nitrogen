% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (cookie_handler).
-export ([
	behaviour_info/1,
	get_cookie/2,
	set_cookie/5
]).



% get_cookie(Key, Context, State) -> Value.
% Read a cookie from the browser.
get_cookie(Key, Context) -> 
	_Value = wf_context:apply_return_raw(cookie, get_cookie, [Key], Context).
	
% set_cookie(Key, Value, Path, MinutesToLive, Context, State) -> {ok, NewContext, NewState}.
% Send a cookie to the browser.
set_cookie(Key, Value, Path, MinutesToLive, Context) -> 
	{ok, _NewContext} = wf_context:apply(cookie, set_cookie, [Key, Value, Path, MinutesToLive], Context).

behaviour_info(callbacks) -> [
	{init, 2},      
	{finish, 2},
	{get_cookie, 3},
	{set_cookie, 6}
];

behaviour_info(_) -> undefined.
