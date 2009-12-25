% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_droppable).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, droppable).

render_element(HtmlID, Record) -> 
	% Get properties...
	Delegate = Record#droppable.delegate,
	Tag = Record#droppable.tag,
	Anchor = wf_context:anchor(),
	PostbackInfo = wf_event:serialize_event_context({Delegate, Tag}, Anchor, Anchor, Anchor, ?MODULE),
	ActiveClass = Record#droppable.active_class, 
	HoverClass = Record#droppable.hover_class,
	AcceptGroups = groups_to_accept(Record#droppable.accept_groups),

	% Write out the script to make this element droppable...
	Script = #script {
		script=wf:f("Nitrogen.$droppable('~s', { activeClass: '~s', hoverClass: '~s', accept: '~s' }, '~s');", [Anchor, ActiveClass, HoverClass, AcceptGroups, PostbackInfo])
	},
	wf:wire(Script),

	% Render as a panel.
	element_panel:render_element(HtmlID, #panel {
		class=[droppable|Record#droppable.class],
		style=Record#droppable.style,
		body=Record#droppable.body
	}).
	
event({Delegate, DropTag}) ->
	DragItem = wf:q(drag_item),
	DragTag = wf:depickle(DragItem),
	Module = wf:coalesce([Delegate, wf:page_module()]),
	Module:drop_event(DragTag, DropTag).

groups_to_accept(all) -> "*";
groups_to_accept(undefined) -> "*";
groups_to_accept(none) -> "";
groups_to_accept([]) -> "*";
groups_to_accept(Groups) ->
	Groups1 = lists:flatten([Groups]),
	Groups2 = [".drag_group_" ++ wf:to_list(X) || X <- Groups1],
	string:join(Groups2, ", ").
