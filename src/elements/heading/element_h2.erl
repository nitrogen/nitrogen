% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_h2).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, h2).

render(ControlID, Record) -> 
	wf:f("<h2 id='~s' class='h2 ~s' style='~s'>~s</h2>", [
		ControlID,
		Record#h2.class,
		Record#h2.style,
		wf:html_encode(Record#h2.text, Record#h2.html_encode)
	]).