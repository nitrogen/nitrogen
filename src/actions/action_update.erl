% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_update).
-include ("wf.inc").
-compile(export_all).

% This action is used internally by Nitrogen.
render_action(Record) ->
	FormatString = case Record#update.type of
		update        -> "Nitrogen.$update(obj('me'), \"~s\");";
		replace       -> "Nitrogen.$replace(obj('me'), \"~s\");";
		insert_top    -> "Nitrogen.$insert_top(obj('me'), \"~s\");";
		insert_bottom -> "Nitrogen.$insert_bottom(obj('me'), \"~s\");"
	end,
	
	% If this is a replacement, then pop up one layer in the current path
	% before rendering...
	OldPath = wf_context:current_path(),
	case Record#update.type == replace of
		true ->  wf_context:current_path(tl(OldPath));
		false -> continue
	end,

	% Render into HTML and Javascript...
	Elements = Record#update.elements,
	{ok, Html, Script} = wf_render:render(Elements, []), 
	
	% Move back to original path...
	wf_context:current_path(OldPath),
	
	% Turn the HTML into a Javascript statement that will update the right element.
	ScriptifiedHtml = wf:f(FormatString, [wf:js_escape(Html)]),
	[ScriptifiedHtml, Script].
	
update(TargetID, Elements) -> 
	update(update, TargetID, Elements).

replace(TargetID, Elements) ->
	update(replace, TargetID, Elements).

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
	
	case Type == update orelse Type == replace of 
		true -> remove_update(TargetID);
		false -> ignore
	end,
	wf_context:add_action(Action),
	ok.

remove_update(TargetID) ->
	Actions = wf_context:actions(),
	
inner_remove_update([Action|Actions], TargetID) ->
	case Action of
		_ when is_list(Action) -> [inner_remove_update(Action, TargetID)|inner_remove_update(Actions, TargetID)];
		_ when is_record(Action, update) -> 
		_ -> [Action|inner_remove_update(Actions, TargetID)]
	end.