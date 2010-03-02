% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_span).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, span).

render_element(Record) -> 
    Text = wf:html_encode(Record#span.text, Record#span.html_encode),
    wf_tags:emit_tag(span, Text, [
        {class, Record#span.class}, 
        {style, Record#span.style}
    ]).
