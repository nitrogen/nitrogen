% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_sortblock).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, sortblock).

render(ControlID, Record) -> 
	% Get properties...
	
	PickledPostbackInfo = action_event:make_postback_info(Record#sortblock.tag, sort, ControlID, ControlID, ?MODULE),
	Handle = case Record#sortblock.handle of
		undefined -> "null";
		Other -> wf:f("'.~s'", [Other])
	end,
	ConnectWithGroups = groups_to_connect_with(Record#sortblock.connect_with_groups),
	GroupClasses = groups_to_classes(Record#sortblock.group),
		
	% Emit the javascript...
	Script = wf:f("wf_sortblock(obj('~s'), { handle: ~s, connectWith: [~s] }, '~s');", [
		ControlID, 
		Handle, 
		ConnectWithGroups,
		PickledPostbackInfo
	]),
	wf:wire(Script),

	element_panel:render(ControlID, #panel {
		class="sortblock " ++ GroupClasses ++ " " ++ wf:to_list(Record#sortblock.class),
		style=Record#sortblock.style,
		body=Record#sortblock.items
	}).

event(BlockTag) ->
	[SortItems] = wf:q(sort_items),
	SortTags = [wf:depickle(X) || X <- string:tokens(SortItems, ",")],
	Module = wf_platform:get_page_module(),
	Module:sort_event(BlockTag, SortTags).

groups_to_classes([]) -> "";
groups_to_classes(undefined) -> "";
groups_to_classes(Groups) ->
	Groups1 = lists:flatten([Groups]),
	Groups2 = ["drag_group_" ++ wf:to_list(X) || X <- Groups1],
	string:join(Groups2, " ").
	
groups_to_connect_with(all) -> "'*'";
groups_to_connect_with(undefined) -> "'*'";
groups_to_connect_with(none) -> "";
groups_to_connect_with([]) -> "'*'";
groups_to_connect_with(Groups) ->
	Groups1 = lists:flatten([Groups]),
	Groups2 = ["'.drag_group_" ++ wf:to_list(X) ++ "'" || X <- Groups1],
	string:join(Groups2, ", ").
