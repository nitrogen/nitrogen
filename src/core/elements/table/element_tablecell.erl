% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_tablecell).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, tablecell).

render(ControlID, Record) -> 
	Content = [
		wf:html_encode(Record#tablecell.text, Record#tablecell.html_encode),
		wf:render(Record#tablecell.body)
	],
	
	wf_tags:emit_tag(td, Content, [
		{id, ControlID},
		{class, [tablecell, Record#tablecell.class]},
		{style, Record#tablecell.style},
		{align, Record#tablecell.align},
		{valign, Record#tablecell.valign},
		{colspan, Record#tablecell.colspan},
		{rowspan, Record#tablecell.rowspan}	
	]).