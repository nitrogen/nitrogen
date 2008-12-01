% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_h1).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, h1).

render(ControlID, Record) -> 
	wf:f("<h1 id='~s' class='h1 ~s' style='~s'>~s</h1>", [
		ControlID,
		Record#h1.class,
		Record#h1.style,
		wf:html_encode(Record#h1.text, Record#h1.html_encode)
	]).