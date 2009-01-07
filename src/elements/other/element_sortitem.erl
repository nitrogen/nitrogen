% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_sortitem).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, sortitem).

render(ControlID, Record) -> 
	PickledTag = wf_utils:pickle(Record#sortitem.tag),
	Script = wf:f("wf_sortitem(obj('~s'), '~s');", [
		ControlID, 
		PickledTag
	]),
	wf:wire(Script),

	element_panel:render(ControlID, #panel {
		class="sortitem " ++ wf:to_list(Record#sortitem.class),
		style=Record#sortitem.style,
		body=Record#sortitem.body
	}).
	