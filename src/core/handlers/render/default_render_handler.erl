% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_render_handler).
-behaviour (render_handler).
-include ("wf.inc").
-include ("simplebridge.hrl").
-export ([
	init/1, 
	finish/2,
	render/4
]).

init(Context) -> 
	{ok, Context, []}.
	
finish(Context, State) -> 
	{ok, Context, State}.

render(Elements, Actions, Context, State) ->
	% First, render the elements.
	{ok, Html, Context1} = render_handler_utils:render_elements(Elements, Context),
	
	% Second, render any actions.
	{ok, Script1, Context2} = render_handler_utils:render_actions(Actions, Context1),
	
	% Third, render queued actions that were a result of step 1 or 2.
	QueuedActions = lists:reverse(Context2#context.queued_actions),
	{ok, Script2, Context3} = render_handler_utils:render_actions(QueuedActions, Context2),
	
	% Return.
	Script=[Script1, Script2],
	{ok, Html, Script, Context3, State}.