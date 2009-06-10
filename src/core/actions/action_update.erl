% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_update).
-include ("wf.inc").
-compile(export_all).

% This action is used internally by Nitrogen.
render_action(Record, Context) ->
	% TODO - Add a 'replace' case here eventually...
	FormatString = case Record#update.type of
		update        -> "Nitrogen.$update(obj('me'), \"~s\");";
		insert_top    -> "Nitrogen.$insert_top(obj('me'), \"~s\");";
		insert_bottom -> "Nitrogen.$insert_bottom(obj('me'), \"~s\");"
	end,

	% Render into HTML and Javascript...
	Elements = Record#update.elements,
	{ok, Html, Script, Context1} = wf_render:render(Elements, [], Context), 
	
	% Turn the HTML into a Javascript statement that will update the right element.
	ScriptifiedHtml = wff:f(FormatString, [wf_utils:js_escape(Html)]),
	{ok, [ScriptifiedHtml, Script], Context1}.
	
update(TargetID, Elements, Context) -> 
	update(update, TargetID, Elements, Context).

insert_top(TargetID, Elements, Context) -> 
	update(insert_top, TargetID, Elements, Context).

insert_bottom(TargetID, Elements, Context) -> 
	update(insert_bottom, TargetID, Elements, Context).

%%% PRIVATE FUNCTIONS %%%

update(Type, TargetID, Elements, Context) ->
	Action = #update {
		type=Type,
		target=TargetID,
		elements=Elements		
	},
	QueuedActions = [Action|Context#context.queued_actions],
	{ok, Context#context { queued_actions = QueuedActions }}.
