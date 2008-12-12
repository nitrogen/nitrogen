% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_h4).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, h4).

render(ControlID, Record) -> 
	wf:f("<h4 id='~s' class='h4 ~s' style='~s'>~s</h4>", [
		ControlID,
		Record#h4.class,
		Record#h4.style,
		Record#h4.text
	]).