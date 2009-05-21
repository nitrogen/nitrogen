% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (page_render_handler).
-behaviour (route_handler).
-include ("wf.inc").
-include ("simplebridge.hrl").
-export ([
	init/1, 
	finish/2,
	render_firstrequest/2,
	render_postback/2
]).

init(Context) -> 
	{ok, Context, []}.
	
finish(Context, State) -> 
	{ok, Context, State}.

render_firstrequest(Context, State) ->
	Bridge = Context#context.response,
	Bridge1 = Bridge:data("Hello world from page_render_handler!"),
	{ok, Context, State, Bridge1}.

render_postback(Context, State) ->
	Bridge = Context#context.response,
	Bridge1 = Bridge:data("Hello world from page_render_handler!"),
	{ok, Context, State, Bridge1}.
