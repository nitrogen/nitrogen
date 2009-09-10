% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_value).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, value).

render_element(HtmlID, Record) -> 
	Text = wf:html_encode(Record#value.text, Record#value.html_encode),
	wf_tags:emit_tag(span, Text, [
		{id, HtmlID},
		{class, [value, Record#value.class]},
		{style, Record#value.style}
	]).