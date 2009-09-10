% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_update).
-include ("wf.inc").
-compile(export_all).

% This action is used internally by Nitrogen.
render_action(Record) ->
	% TODO - Add a 'replace' case here eventually...
	FormatString = case Record#update.type of
		update        -> "Nitrogen.$update(obj('me'), \"~s\");";
		insert_top    -> "Nitrogen.$insert_top(obj('me'), \"~s\");";
		insert_bottom -> "Nitrogen.$insert_bottom(obj('me'), \"~s\");"
	end,

	% Render into HTML and Javascript...
	Elements = Record#update.elements,
	{ok, Html, Script} = wf_render:render(Elements, []), 
	
	% Turn the HTML into a Javascript statement that will update the right element.
	ScriptifiedHtml = wf:f(FormatString, [wf:js_escape(Html)]),
	[ScriptifiedHtml, Script].
	
update(TargetID, Elements) -> 
	update(update, TargetID, Elements).

insert_top(TargetID, Elements) -> 
	update(insert_top, TargetID, Elements).

insert_bottom(TargetID, Elements) -> 
	update(insert_bottom, TargetID, Elements).

%%% PRIVATE FUNCTIONS %%%

update(Type, TargetID, Elements) ->
	CurrentPath = wf_context:current_path(),
	Action = #wire { target=CurrentPath, actions=[
		#update {
			type=Type,
			target=TargetID,
			elements=Elements		
		}
	]},
	wf_context:add_action(Action),
	ok.
