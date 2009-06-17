% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_link).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, link).

render_element(HtmlID, Record, Context) -> 
	{ok, Context1} = case Record#link.postback of
		undefined -> {ok, Context};
		Postback -> wff:wire(Record#link.id, #event { type=click, postback=Postback })
	end,
	
	Body = [
		wf:html_encode(Record#link.text, Record#link.html_encode),
		Record#link.body
	],
	
	Elements = wf_tags:emit_tag(a, Body, [
		{id, HtmlID},
		{href, Record#link.url},
		{class, [link, Record#link.class]},
		{style, Record#link.style}
	]),
	
	{ok, Elements, Context1}.