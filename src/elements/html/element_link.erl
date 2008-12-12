% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_link).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, link).

render(ControlID, Record) -> 
	case Record#link.postback of
		undefined -> ok;
		Postback -> wf:wire(ControlID, #event { type=click, postback=Postback })
	end,
	wf:f("<a id='~s' href='~s' class='link ~s' style='~s'>~s</a>", [
		ControlID,
		Record#link.url,
		Record#link.class,
		Record#link.style,
		wf:html_encode(Record#link.text, Record#link.html_encode)
	]).