% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_render).
-include ("wf.inc").
-export ([
	render/3
]).

render(Elements, Actions, Context) ->
	{ok, Context1} = update_context_with_dom_paths(Context),
	{ok, _Html, _Script, _Context2} = render2(Elements, Actions, Context1).

update_context_with_dom_paths(Context) when Context#context.dom_paths /= undefined ->
	{ok, Context};
	
update_context_with_dom_paths(Context) when Context#context.dom_paths == undefined ->
	% Store the list of paths...
	DomPaths = wff:q(domPaths, Context),
	Page = Context#context.page_context,
	BasePath = [wff:to_list(Page#page_context.name)],
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
	{ok, Context1}.

render2(Elements, Actions, Context) ->
	% Save any queued actions...
	OldQueuedActions = Context#context.queued_actions,
	Context1 = Context#context { queued_actions=[] },

	% First, render the elements.
	{ok, Html, Context2} = wf_render_elements:render_elements(Elements, Context1),
	
	% Second, render any actions.
	{ok, Script1, Context3} = wf_render_actions:render_actions(Actions, Context2),
	
	% Third, render queued actions that were a result of step 1 or 2.
	QueuedActions = lists:reverse(Context2#context.queued_actions),
	{ok, Script2, Context4} = wf_render_actions:render_actions(QueuedActions, Context3),
	
	% Restore queued actions...
	Context5 = Context4#context { queued_actions=OldQueuedActions },
	
	% Return.
	Script=[Script1, Script2],
	{ok, Html, Script, Context5}.

	