% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_h4).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, h4).

render(ControlID, Record) -> 
	Content = wf:html_encode(Record#h4.text, Record#h4.html_encode),
	wf_tags:emit_tag(h4, Content, [
		{id, ControlID},
		{class, [h4, Record#h4.class]},
		{style, Record#h4.style}
	]).
