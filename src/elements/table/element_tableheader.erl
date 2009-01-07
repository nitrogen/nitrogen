% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_tableheader).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, tableheader).

render(ControlID, Record) -> 
	Content = [
		wf:html_encode(Record#tableheader.text, Record#tableheader.html_encode),
		wf:render(Record#tableheader.body)
	],
	
	wf_tags:emit_tag(th, Content, [
		{id, ControlID},
		{class, [tableheader, Record#tableheader.class]},
		{style, Record#tableheader.style},
		{align, Record#tableheader.align},
		{valign, Record#tableheader.valign},
		{colspan, Record#tableheader.colspan},
		{rowspan, Record#tableheader.rowspan}
	]).