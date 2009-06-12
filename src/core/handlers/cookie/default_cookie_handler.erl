% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_cookie_handler).
-behaviour (cookie_handler).
-include ("wf.inc").
-export ([
	init/2, 
	finish/2,
	get_cookie/3,
	set_cookie/6
]).

init(Context, _State) -> 
	% Get cookies from the request bridge...
	Bridge = Context#context.request,
	Cookies = Bridge:cookies(),

	% Load into state...
	{ok, Context, [Cookies, []]}.
	
finish(Context, [_Cookies, NewCookies]) -> 
  % Get the response...
	Response = Context#context.response,
	
	% Fold over each new cookie, adding it to the response...
	F = fun({Key, Value, Path, MinutesToLive}, Rsp) ->
		Rsp:cookie(Key, Value, Path, MinutesToLive)
	end,
	Response1 = lists:foldl(F, Response, NewCookies),
	
	% Return a context with the new response...
	Context1 = Context#context { response = Response1 },
	{ok, Context1, []}.

get_cookie(Key, _Context, [Cookies, _NewCookies]) -> 
	proplists:get_value(Key, Cookies, undefined).
	
set_cookie(Key, Value, Path, MinutesToLive, Context, [Cookies, NewCookies]) -> 
	NewCookie = {Key, Value, Path, MinutesToLive},
	{ok, Context, [Cookies, [NewCookie|NewCookies]]}.