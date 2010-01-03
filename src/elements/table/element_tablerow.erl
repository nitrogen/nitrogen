% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_tablerow).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, tablerow).

render_element(Record) -> 
	Cells = Record#tablerow.cells,
	wf_tags:emit_tag(tr, Cells, [
		% {id, HtmlID},
		{class, [tablerow, Record#tablerow.class]},
		{style, Record#tablerow.style}
	]).