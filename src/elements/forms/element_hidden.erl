% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_hidden).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, hidden).

render(ControlID, Record) -> 
	wf:f("<input id='~s' type='hidden' name='~s' value=\"~s\" />", [
		ControlID, 
		ControlID, 
		wf:html_encode(Record#hidden.text, Record#hidden.html_encode)
	]).
	
