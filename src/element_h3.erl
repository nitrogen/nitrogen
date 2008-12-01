% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_h3).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, h3).

render(ControlID, Record) -> 
	wf:f("<h3 id='~s' class='h3 ~s' style='~s'>~s</h3>", [
		ControlID,
		Record#h3.class,
		Record#h3.style,
		wf:html_encode(Record#h3.text, Record#h3.html_encode)
	]).