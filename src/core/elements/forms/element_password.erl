% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_password).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, password).

render_element(HtmlID, Record, Context) -> 
	{ok, Context1} = case Record#password.next of
		undefined -> {ok, Context};
		Next -> wff:wire(Record#password.id, #event { type=enterkey, actions=wf:f("Nitrogen.$go_next('~s');", [Next]) }, Context)
	end,
	{ok, Context2} = case Record#password.postback of
		undefined -> {ok, Context1};
		Postback -> wff:wire(Record#password.id, #event { type=enterkey, postback=Postback }, Context1)
	end,

	Value = wf:html_encode(Record#password.text, Record#password.html_encode),
	Elements = wf_tags:emit_tag(input, [
		{id, HtmlID},
		{id, HtmlID},
		{type, password},
		{class, [password, Record#password.class]},
		{style, Record#password.style},
		{value, Value}
	]),
	{ok, Elements, Context2}.