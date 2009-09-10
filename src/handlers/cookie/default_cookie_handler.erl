% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

% Though this is defined as a handler, it is unlikely
% that anyone would want to override the default behaviour. 
% It is defined as a handler simply because it fit well 
% into the existing handler pattern.

-module (default_cookie_handler).
-behaviour (cookie_handler).
-include ("wf.inc").
-export ([
	init/1, 
	finish/1,
	get_cookie/2,
	set_cookie/5
]).

init(_State) -> 
	% Get cookies from the request bridge...
	% Request = wf_context:request_bridge(),
	% TODO - Cookies = Request:cookies(),
	Cookies = [],
	
	% Load into state...
	{ok, [Cookies, []]}.
	
finish([_Cookies, _NewCookies]) -> 
  % Get the response...
  % TODO
	% Response = wf_context:response_bridge(),
	% 
	% % Fold over each new cookie, adding it to the response...
	% F = fun({Key, Value, Path, MinutesToLive}, Rsp) ->
	% 	Rsp:cookie(Key, Value, Path, MinutesToLive)
	% end,
	% Response1 = lists:foldl(F, Response, NewCookies),
	% 
	% % Return a context with the new response...
	% wf_context:response_bridge(Response1),
	{ok, []}.

get_cookie(Key, [Cookies, _NewCookies]) -> 
	proplists:get_value(Key, Cookies, undefined).
	
set_cookie(Key, Value, Path, MinutesToLive, [Cookies, NewCookies]) -> 
	NewCookie = {Key, Value, Path, MinutesToLive},
	{ok, [Cookies, [NewCookie|NewCookies]]}.