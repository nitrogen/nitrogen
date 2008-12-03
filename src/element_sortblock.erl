% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_sortblock).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, sortblock).

render(ControlID, Record) -> 
	% Get the postbackinfo...
	PickledPostbackInfo = action_event:make_postback_info(Record#sortblock.postback, sort, ControlID, ControlID, ?MODULE),
	Handle = case Record#sortblock.handle of
		null -> "null";
		Other -> wf:f("'.~s'", [Other])
	end,
	ConnectWith = lists:flatten([Record#sortblock.connect_with]),
	ConnectWith1 = [wf:f("'#' + obj('~s').id", [X]) || X <- ConnectWith],
	ConnectWith2 = wf:f("[~s]", [string:join(ConnectWith1, ", ")]),
		
	% Emit the javascript...
	Script = [
		wf:f("obj('~s').wf_sort_postback='~s';", [ControlID, PickledPostbackInfo]),
		wf:f("wf_sortable(obj('~s'), { handle: ~s, connectWith: ~s });", [ControlID, Handle, ConnectWith2])
	],
	wf:wire(Script),

	element_panel:render(ControlID, #panel {
		class="sortblock " ++ wf:to_list(Record#sortblock.class),
		style=Record#sortblock.style,
		body=Record#sortblock.body
	}).

event(Postback) ->
	[SortItems] = wf:q(sort_items),
	SortTags = [wf:depickle(X) || X <- string:tokens(SortItems, ",")],
	Module = wf_platform:get_page_module(),
	Module:sort_event(Postback, SortTags).
