% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_button).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, button).

render(ControlID, Record) -> 
	case Record#button.postback of
		undefined -> ok;
		Postback -> wf:wire(ControlID, #event { type=click, postback=Postback })
	end,
	wf:f("<input id='~s' class='button ~s' style='~s' type='button' name='~s' value='  ~s  '  />", [
		ControlID, 
		Record#button.class,
		Record#button.style,
		ControlID, 
		wf:html_encode(Record#button.text, Record#button.html_encode)
	]).