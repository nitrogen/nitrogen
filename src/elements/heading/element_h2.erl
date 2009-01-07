% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_h2).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, h2).

render(ControlID, Record) -> 
	Content = Record#h2.text,
	wf_tags:emit_tag(h2, Content, [
		{id, ControlID},
		{class, [h2, Record#h2.class]},
		{style, Record#h2.style}
	]).