% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (static_route_handler).
-behaviour (route_handler).
-include ("wf.inc").
-include ("simplebridge.hrl").
-export ([
	init/2, 
	finish/2
]).

init(Context, Module) -> 
	% Some values...
	Bridge = Context#context.request,
	Page = Context#context.page_context,
	Path = Bridge:path(),
	
	% Update the page_context with the path and module.
	Page1 = Page#page_context {
		module=Module,
		path_info=Path
	},
	Context1 = Context#context { page_context=Page1 },
	{ok, Context1, Module}.
		
finish(Context, Module) -> 
	{ok, Context, Module}.
