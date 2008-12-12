% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_label).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, label).

render(ControlID, Record) -> 
	wf:f("<span id='~s' class='label ~s' style='~s'>~s</span>", [
		ControlID,
		Record#label.class,
		Record#label.style,
		wf:html_encode(Record#label.text, Record#label.html_encode)
	]).