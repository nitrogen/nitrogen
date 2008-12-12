% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_br).
-compile(export_all).
-include ("wf.inc").

reflect() -> record_info(fields, br).

render(ControlID, Record) -> 
	wf:f("<br id='~s' class='p ~s' style='~s'>", [
		ControlID, 
		Record#br.class,
		Record#br.style
	]).