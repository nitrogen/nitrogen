% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_listitem).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, listitem).

render_element(ControlID, Record, Context) -> 
	Body = [
		wff:html_encode(Record#listitem.text, Record#listitem.html_encode),
		Record#listitem.body
	],

	Elements = wf_tags:emit_tag(li, Body, [
		{id, ControlID},
		{class, [listitem, Record#listitem.class]},
		{style, Record#listitem.style}
	]),
	
	{ok, Elements, Context}.
