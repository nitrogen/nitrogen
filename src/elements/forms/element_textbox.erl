% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_textbox).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, textbox).

render(ControlID, Record) -> 
	case Record#textbox.next of
		undefined -> ok;
		Next -> wf:wire(ControlID, #event { type=enterkey, actions=wf:f("wf_go_next('~s');", [Next]) })
	end,
	case Record#textbox.postback of
		undefined -> ok;
		Postback -> wf:wire(ControlID, #event { type=enterkey, postback=Postback })
	end,
	
	Value = wf:html_encode(Record#textbox.text, Record#textbox.html_encode),
	wf_tags:emit_tag(input, [
		{id, ControlID}, 
		{name, ControlID},
		{type, text}, 
		{class, [textbox, Record#textbox.class]},
		{style, Record#textbox.style},
		{value, Value}
	]).