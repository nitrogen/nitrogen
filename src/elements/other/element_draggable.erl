% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_draggable).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, draggable).

render(ControlID, Record) -> 
	% Get properties...
	PickledTag = wf_utils:pickle(Record#draggable.tag),
	
	GroupClasses = groups_to_classes(Record#draggable.group),

	Handle = case Record#draggable.handle of
		undefined -> "null";
		Other2 -> wf:f("'.~s'", [Other2])
	end,

	Helper = case Record#draggable.clone of
		true -> clone;
		false -> original
	end,
	
	Revert = case Record#draggable.revert of
		true -> "true";
		false -> "false";
		valid -> "'valid'";
		invalid -> "'invalid'"
	end,

	% Write out the script to make this element draggable...
	Script = wf:f("wf_draggable(obj('~s'), { handle: ~s, helper: '~s', revert: ~s }, '~s');", [
		ControlID, 
		Handle, 
		Helper, 
		Revert, 
		PickledTag
	]),
	wf:wire(Script),

	% Render as a panel...
	element_panel:render(ControlID, #panel {
		class="draggable " ++ GroupClasses ++ " " ++ wf:to_list(Record#draggable.class),
		style=Record#draggable.style,
		body=Record#draggable.body
	}).
	
groups_to_classes([]) -> "";
groups_to_classes(undefined) -> "";
groups_to_classes(Groups) ->
	Groups1 = lists:flatten([Groups]),
	Groups2 = ["drag_group_" ++ wf:to_list(X) || X <- Groups1],
	string:join(Groups2, " ").
	
