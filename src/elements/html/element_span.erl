% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_span).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, span).

%%% CODE %%%

render(ControlID, Record) -> 
	wf:f("<span id='~s' class='span ~s' style='~s'>~s</span>", [
		ControlID,
		Record#span.class,
		Record#span.style,
		wf:html_encode(Record#span.text, Record#span.html_encode)
	]).