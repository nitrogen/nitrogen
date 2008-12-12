% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_p).
-compile(export_all).
-include ("wf.inc").

reflect() -> record_info(fields, p).

render(ControlID, Record) -> 
	wf:f("<p id='~s' class='p ~s' style='~s' />", [
		ControlID, 
		Record#p.class,
		Record#p.style
	]).