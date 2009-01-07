% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_label).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, label).

render(ControlID, Record) -> 
	Content = wf:html_encode(Record#label.text, Record#label.html_encode),
	wf_tags:emit_tag(span, Content, [
		{id, ControlID},
		{class, [label, Record#label.class]},
		{style, Record#label.style}
	]).