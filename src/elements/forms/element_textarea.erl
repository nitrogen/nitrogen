% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_textarea).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, textarea).

render(ControlID, Record) -> 
	wf:f("<textarea id='~s' class='textarea ~s' style='~s' name='~s'>~s</textarea>", [
		ControlID, 
		Record#textarea.class,
		Record#textarea.style,
		ControlID, 
		wf:html_encode(Record#textarea.text, Record#textarea.html_encode)
	]).
	
