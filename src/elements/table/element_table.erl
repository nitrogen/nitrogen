% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_table).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, table).

render(ControlID, Record) -> 
	Content = wf:render(Record#table.rows),
	
	wf_tags:emit_tag(table, Content, [
		{id, ControlID},
		{border, 0},
		{cellpadding, 0},
		{cellspacing, 0},
		{class, [table, Record#table.class]},
		{style, Record#table.style}
	]).