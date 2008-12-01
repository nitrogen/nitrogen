% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_table).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, table).

render(ControlID, Record) -> 
	[
		wf:f("<table border=0 cellpadding=0 cellspacing=0 id='~s' class='table ~s' style='~s'>", [
			ControlID,
			Record#table.class,
			Record#table.style
		]),
		wf:render(Record#table.rows),
		"</table>"
	].