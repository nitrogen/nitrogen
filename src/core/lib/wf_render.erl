% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_render).
-include ("wf.inc").
-export ([
	render/2
]).

render(Elements, Actions) ->
	% Save any queued actions...
	OldQueuedActions = wf_context:actions(),
	wf_context:clear_actions(),

	% First, render the elements.
	{ok, Html} = wf_render_elements:render_elements(Elements),
	
	% Second, render any actions.
	{ok, Script1} = wf_render_actions:render_actions(Actions),
	
	% Third, render queued actions that were a result of step 1 or 2.
	QueuedActions = wf_context:actions(),
	{ok, Script2} = wf_render_actions:render_actions(QueuedActions),
	
	% Restore queued actions...
	wf_context:actions(OldQueuedActions),
	
	% Return.
	Script=[Script1, Script2],
	{ok, Html, Script}.

	