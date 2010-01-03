% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_password).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, password).

render_element(Record) -> 
	case Record#password.next of
		undefined -> ignore;
		Next -> wf:wire(Record#password.id, #event { type=enterkey, actions=wf:f("Nitrogen.$go_next('~s');", [Next]) })
	end,
	case Record#password.postback of
		undefined -> ignore;
		Postback -> wf:wire(Record#password.id, #event { type=enterkey, postback=Postback, delegate=Record#password.delegate })
	end,

	Value = wf:html_encode(Record#password.text, Record#password.html_encode),
	wf_tags:emit_tag(input, [
		{type, password},
		{class, [password, Record#password.class]},
		{style, Record#password.style},
		{value, Value}
	]).