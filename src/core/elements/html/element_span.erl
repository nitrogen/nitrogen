% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_span).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, span).

%%% CODE %%%

render(ControlID, Record) -> 
  Contents = wf:html_encode(Record#span.text, Record#span.html_encode),
	wf_tags:emit_tag(span, Contents, [
        {id, ControlID},
        {class, Record#span.class}, 
        {style, Record#span.style}
    ]).

render_element(ControlID, Record, Context) -> 
  Text = wf:html_encode(Record#span.text, Record#span.html_encode),
	Html = wf_tags:emit_tag(span, Text, [
        {id, ControlID},
        {class, Record#span.class}, 
        {style, Record#span.style}
  ]),
	{ok, Html, Context}.