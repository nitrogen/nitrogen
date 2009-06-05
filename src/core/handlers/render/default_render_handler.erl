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
	% Store the list of paths...
	DomPaths = wff:q(domPaths, Context),
	BasePath = Context#context.current_path,
	Context1 = case DomPaths of
		undefined -> 
			Context#context {
				dom_paths=[BasePath]
			};

		_ -> 
			DomPathList = string:tokens(DomPaths, ","),
			DomPathList1 = [lists:reverse(string:tokens(X, "__")) || X <- DomPathList],
			Context#context {
				dom_paths=[BasePath|DomPathList1]
			}
	end,
	{ok, Context1, []}.
	
finish(Context, State) -> 
	{ok, Context, State}.

render(Elements, Actions, Context, State) ->
	% Save queued actions. (This allows 
	OldQueuedActions = Context#context.queued_actions,
	Context1 = Context#context { queued_actions=[] },
		
	% First, render the elements.
	{ok, Html, Context2} = render_handler_utils:render_elements(Elements, Context1),
	
	% Second, render any actions.
	{ok, Script1, Context3} = render_handler_utils:render_actions(Actions, Context2),
	
	% Third, render queued actions that were a result of step 1 or 2.
	QueuedActions = lists:reverse(Context2#context.queued_actions),
	{ok, Script2, Context4} = render_handler_utils:render_actions(QueuedActions, Context3),
	
	% Restore queued actions...
	Context5 = Context4#context { queued_actions=OldQueuedActions },
	
	% Return.
	Script=[Script1, Script2],
	{ok, Html, Script, Context5, State}.