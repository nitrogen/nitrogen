% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_tablerow).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, tablerow).

render(ControlID, Record) -> 
	[
		wf:f("<tr id='~s' class='tablerow ~s' style='~s'>", [
			ControlID,
			Record#tablerow.class,
			Record#tablerow.style
		]),
		wf:render(Record#tablerow.cells),
		"</tr>"
	].