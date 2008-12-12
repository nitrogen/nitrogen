% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_checkbox).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, checkbox).

render(ControlID, Record) -> 
	CheckedString = case Record#checkbox.checked of
		true -> "checked=true";
		_ -> []
	end,
	case Record#checkbox.postback of
		undefined -> ok;
		Postback -> wf:wire(ControlID, #event { type=change, postback=Postback })
	end,
	wf:f("<input id='~s' class='checkbox ~s' style='~s' type='checkbox' name='~s' ~s><label for='~s'>~s</label>", [
		ControlID, 
		Record#checkbox.class,
		Record#checkbox.style,
		ControlID, 
		CheckedString,
		ControlID,
		wf:html_encode(Record#checkbox.text, Record#checkbox.html_encode)
	]).