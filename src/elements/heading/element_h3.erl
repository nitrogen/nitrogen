% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_h3).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, h3).

render(ControlID, Record) -> 
	Content = Record#h3.text,
	wf_tags:emit_tag(h3, Content, [
		{id, ControlID},
		{class, [h3, Record#h3.class]},
		{style, Record#h3.style}
	]).