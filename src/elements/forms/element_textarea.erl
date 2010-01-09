% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_textarea).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, textarea).

render_element(Record) -> 
	Text = wf:html_encode(Record#textarea.text, Record#textarea.html_encode),
	wf_tags:emit_tag(textarea, Text, [
		{class, [textarea, Record#textarea.class]},
		{style, Record#textarea.style}
	]).