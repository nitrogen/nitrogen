% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_password).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, password).

render(ControlID, Record) -> 
	case Record#password.next of
		undefined -> ok;
		Next -> wf:wire(ControlID, #event { type=enterkey, actions=wf:f("wf_go_next('~s');", [Next]) })
	end,
	case Record#password.postback of
		undefined -> ok;
		Postback -> wf:wire(ControlID, #event { type=enterkey, postback=Postback })
	end,
	wf:f("<input id='~s' class='password ~s' style='~s' type='password' name='~s' value='~s' />", [
		ControlID, 
		Record#password.class,
		Record#password.style,
		ControlID, 
		wf:html_encode(Record#password.text, Record#password.html_encode)
	]).