% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_list).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, list).

render_element(Record) -> 
	Tag = case Record#list.numbered of 
		true -> ol;
		_ -> ul
	end,

	wf_tags:emit_tag(Tag, Record#list.body, [
		{class, [list, Record#list.class]},
		{style, Record#list.style}
	]).