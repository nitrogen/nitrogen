% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_tableheader).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, tableheader).

render(ControlID, Record) -> 
	[
		wf:f("<th id='~s' class='tableheader ~s' style='~s' align='~s' valign='~s' colspan='~b' rowspan='~b'>", [
			ControlID,
			Record#tableheader.class,
			Record#tableheader.style,
			Record#tableheader.align,
			Record#tableheader.valign,
			Record#tableheader.colspan,
			Record#tableheader.rowspan
		]),
		wf:html_encode(Record#tableheader.text, Record#tableheader.html_encode),
		wf:render(Record#tableheader.body),
		"</th>"
	].