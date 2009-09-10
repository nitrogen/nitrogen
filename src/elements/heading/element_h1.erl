% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_h1).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, h1).

render_element(HtmlID, Record) -> 
	Text = wf:html_encode(Record#h1.text, Record#h1.html_encode),
	wf_tags:emit_tag(h1, Text, [
		{id, HtmlID},
		{class, [h1, Record#h1.class]},
		{style, Record#h1.style}
	]).
