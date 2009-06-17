% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_list).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, list).

render_element(HtmlID, Record, Context) -> 
	Tag = case Record#list.numbered of 
		true -> ol;
		_ -> ul
	end,

	Elements = wf_tags:emit_tag(Tag, Record#list.body, [
		{id, HtmlID},
		{class, [list, Record#list.class]},
		{style, Record#list.style}
	]),
	{ok, Elements, Context}.