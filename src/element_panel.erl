% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_panel).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, panel).

render(ControlID, Record) -> 
	[
		wf:f("<div id='~s' class='panel ~s' style='~s'>", [
			ControlID,
			Record#panel.class,
			Record#panel.style
		]),
		wf:render(Record#panel.body),
		"</div>"
	].