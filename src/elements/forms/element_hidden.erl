% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_hidden).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, hidden).

render(ControlID, Record) -> 
	Value = wf:html_encode(Record#hidden.text, Record#hidden.html_encode),
	wf_tags:emit_tag(input, [
		{id, ControlID}, 
		{name, ControlID}, 
		{type, hidden},
		{value, Value}
	]).