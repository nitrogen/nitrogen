% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_h1).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, h1).

render(ControlID, Record) -> 
	Content = Record#h1.text,
	wf_tags:emit_tag(h1, Content, [
		{id, ControlID},
		{class, [h1, Record#h1.class]},
		{style, Record#h1.style}
	]).