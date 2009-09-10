% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_br).
-compile(export_all).
-include ("wf.inc").

reflect() -> record_info(fields, br).

render_element(HtmlID, Record) -> 
	wf_tags:emit_tag(br, [
		{id, HtmlID},
		{class, [br, Record#br.class]}, 
		{style, Record#br.style}
	]).