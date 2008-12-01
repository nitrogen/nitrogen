% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_tablecell).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, tablecell).

render(ControlID, Record) -> 
	[
		wf:f("<td id='~s' class='tablecell ~s' style='~s' align='~s' valign='~s' colspan='~b' rowspan='~b'>", [
			ControlID,
			Record#tablecell.class,
			Record#tablecell.style,
			Record#tablecell.align,
			Record#tablecell.valign,
			Record#tablecell.colspan,
			Record#tablecell.rowspan
		]),
		wf:html_encode(Record#tablecell.text, Record#tablecell.html_encode),
		wf:render(Record#tablecell.body),
		"</td>"
	].