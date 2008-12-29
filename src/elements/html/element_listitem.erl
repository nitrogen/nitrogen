% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_listitem).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, listitem).

render(ControlID, Record) -> 
	[
		wf:f("<li id='~s' class='listitem ~s' style='~s'>", [
			ControlID,
			Record#listitem.class,
			Record#listitem.style
		]),
		wf:html_encode(Record#listitem.text, Record#listitem.html_encode),
		wf:render(Record#listitem.body),
		"</li>"
	].