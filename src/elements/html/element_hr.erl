% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_hr).
-compile(export_all).
-include ("wf.inc").

reflect() -> record_info(fields, hr).

render(ControlID, Record) -> 
	wf:f("<hr size=1 id='~s' class='p ~s' style='~s'>", [
		ControlID, 
		Record#hr.class,
		Record#hr.style
	]).