% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_droppable).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, droppable).

render_element(HtmlID, Record) -> 
	% Get properties...
	Tag = Record#droppable.tag,
	PostbackInfo = wf_event:serialize_event_context(Tag, sort, Record#droppable.id, Record#droppable.id, ?MODULE),
	ActiveClass = Record#droppable.active_class, 
	HoverClass = Record#droppable.hover_class,
	AcceptGroups = groups_to_accept(Record#droppable.accept_groups),

	% Write out the script to make this element droppable...
	Script = #script {
		script=wf:f("Nitrogen.$droppable(obj('me'), { activeClass: '~s', hoverClass: '~s', accept: '~s' }, '~s');", [ActiveClass, HoverClass, AcceptGroups, PostbackInfo])
	},
	wf:wire(Record#droppable.id, Script),

	% Render as a panel.
	element_panel:render_element(HtmlID, #panel {
		class="droppable " ++ wf:to_list(Record#droppable.class),
		style=Record#droppable.style,
		body=Record#droppable.body
	}).
	
event(DropTag) ->
	DragItem = wf:q(drag_item),
	DragTag = wf:depickle(DragItem),
	Module = wf_context:page_module(),
	Module:drop_event(DragTag, DropTag).

groups_to_accept(all) -> "*";
groups_to_accept(undefined) -> "*";
groups_to_accept(none) -> "";
groups_to_accept([]) -> "*";
groups_to_accept(Groups) ->
	Groups1 = lists:flatten([Groups]),
	Groups2 = [".drag_group_" ++ wf:to_list(X) || X <- Groups1],
	string:join(Groups2, ", ").
