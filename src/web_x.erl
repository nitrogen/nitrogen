% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (web_x).
-include ("wf.inc").
-export ([go/0, go/1, link/4, link/5, main/0, event/1]).

get_seconds() -> calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

link(BaseHref, Module, Function, ExtraArgs) -> link(BaseHref, Module, Function, ExtraArgs, 24 * 365 * 60 * 60).

link(BaseHref, Module, Function, ExtraArgs, SecondsToLive) ->
	T = wf:pickle({web_x, get_seconds() + SecondsToLive, Module, Function, ExtraArgs}),
	wf:f("~s?~s", [BaseHref, T]).
	
go() -> go(undefined).
go(Function) ->
	QueryString = wf_platform:get_querystring(),
	case wf:depickle(QueryString) of 
		{web_x, Expiration, Module, Function1, ExtraArgs} when Function==undefined orelse Function==Function1 ->
			IsExpired = Expiration > get_seconds(),
			{ok, erlang:apply(Module, Function1, [IsExpired, ExtraArgs])};

		_ -> not_handled
	end.
	
main() -> 
	case go() of
		{ok, Result} -> Result;
		not_handled -> wf:redirect("/")
	end.
	
event(_) -> ok.