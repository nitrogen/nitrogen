% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_link).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, link).

render(ControlID, Record) -> 
	case Record#link.postback of
		undefined -> ok;
		Postback -> wf:wire(ControlID, #event { type=click, postback=Postback })
	end,
	
	Content = [
		wf:html_encode(Record#link.text, Record#link.html_encode),
		wf:render(Record#link.body)
	],
	
	wf_tags:emit_tag(a, Content, [
		{id, ControlID},
		{href, Record#link.url},
		{class, [link, Record#link.class]},
		{style, Record#link.style}
	]).