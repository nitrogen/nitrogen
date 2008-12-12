% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_dropdown).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, dropdown).

render(ControlID, Record) -> 
	case Record#dropdown.postback of
		undefined -> ok;
		Postback -> wf:wire(ControlID, #event { type=change, postback=Postback })
	end,
	
	case Record#dropdown.value of 
		undefined -> ok;
		Value -> wf:set(ControlID, Value)
	end,
	
	Options=case Record#dropdown.options of
		undefined -> "";
		L -> [create_option(X, Record#dropdown.html_encode) || X <- L]
	end,

	wf:f("<select id='~s' class='dropdown ~s' style='~s' name='~s'>~s</select>", [
		ControlID, 
		Record#dropdown.class,
		Record#dropdown.style,
		ControlID,
		Options
	]).
	
create_option(X, HtmlEncode) ->
	Selected = case X#option.selected of
		true -> "selected=true";
		_ -> ""
	end,
	
	wf:f("<option value=\"~s\" ~s>~s</option>", [
		wf:html_encode(X#option.value, HtmlEncode),
		Selected,
		wf:html_encode(X#option.text, HtmlEncode)
	]).