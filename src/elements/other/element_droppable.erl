% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_droppable).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, droppable).

render(ControlID, Record) -> 
	% Get properties...
	PickledPostbackInfo = action_event:make_postback_info(Record#droppable.tag, sort, ControlID, ControlID, ?MODULE),
	ActiveClass = Record#droppable.active_class, 
	HoverClass = Record#droppable.hover_class,
	AcceptGroups = groups_to_accept(Record#droppable.accept_groups),

	% Write out the script to make this element droppable...
	Script = wf:f("wf_droppable(obj('~s'), { activeClass: '~s', hoverClass: '~s', accept: '~s' }, '~s');", [
		ControlID, 
		ActiveClass, 
		HoverClass, 
		AcceptGroups, 
		PickledPostbackInfo
	]),
	wf:wire(Script),

	% Render as a panel.
	element_panel:render(ControlID, #panel {
		class="droppable " ++ wf:to_list(Record#droppable.class),
		style=Record#droppable.style,
		body=Record#droppable.body
	}).
	
event(DropTag) ->
	[DragItem] = wf:q(drag_item),
	DragTag = wf:depickle(DragItem),
	Module = wf_platform:get_page_module(),
	Module:drop_event(DragTag, DropTag).

groups_to_accept(all) -> "*";
groups_to_accept(undefined) -> "*";
groups_to_accept(none) -> "";
groups_to_accept([]) -> "*";
groups_to_accept(Groups) ->
	Groups1 = lists:flatten([Groups]),
	Groups2 = [".drag_group_" ++ wf:to_list(X) || X <- Groups1],
	string:join(Groups2, ", ").
