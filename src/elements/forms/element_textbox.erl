% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
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
	wf:f("<input id='~s' class='textbox ~s' style='~s' type='text' name='~s' value=\"~s\" />", [
		ControlID, 
		Record#textbox.class,
		Record#textbox.style,
		ControlID, 
		wf:html_encode(Record#textbox.text, Record#textbox.html_encode)
	]).
	
