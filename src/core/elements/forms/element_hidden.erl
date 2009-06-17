% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_hidden).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, hidden).

render_element(HtmlID, Record, Context) -> 
	Value = wf:html_encode(Record#hidden.text, Record#hidden.html_encode),
	Elements = wf_tags:emit_tag(input, [
		{id, HtmlID}, 
		{id, HtmlID}, 
		{type, hidden},
		{value, Value}
	]), 
	{ok, Elements, Context}.