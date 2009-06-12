% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_button).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, button).

render_element(ControlID, Record, Context) ->
	{ok, Context1} = case Record#button.postback of
		undefined -> {ok, Context};
		Postback -> wff:wire(me, #event { type=click, postback=Postback }, Context)
	end,
	
	Value = ["  ", wf:html_encode(Record#button.text, Record#button.html_encode), "  "], 
	Html = wf_tags:emit_tag(input, [
		{id, ControlID},
		{name, ControlID},
		{type, button},
		{class, [button, Record#button.class]},
		{style, Record#button.style},
		{value, Value}
	]),
	{ok, Html, Context1}.