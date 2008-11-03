% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_span).
-include ("wf.inc").
-compile(export_all).

%%% DOCUMENTATION %%%
name() -> "Span".
attributes() -> [id, class, style, text, html_encode].
attribute(text) -> "List or binary text to display on the span.";
attribute(html_encode) -> "Boolean determining whether the text will be HTML encoded. Switch '<' to '&lt;'".

example() -> [
	#span { id=theSpan, style="font-size: 20px;", text="Hello, world." }
].

see_also() -> [element_panel].

%%% CODE %%%

render(ControlID, Record) -> 
	wf:f("<span id='~s' class='span ~s' style='~s'>~s</span>", [
		ControlID,
		Record#span.class,
		Record#span.style,
		wf:html_encode(Record#span.text, Record#span.html_encode)
	]).