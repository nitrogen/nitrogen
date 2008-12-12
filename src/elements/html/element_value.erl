% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_value).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, value).

render(ControlID, Record) -> 
	wf:f("<span id='~s' class='value ~s' style='~s'>~s</span>", [
		ControlID,
		Record#value.class,
		Record#value.style,
		wf:html_encode(Record#value.text, Record#value.html_encode)
	]).