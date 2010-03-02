% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_label).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, label).

render_element(Record) -> 
    Text = wf:html_encode(Record#label.text, Record#label.html_encode),
    wf_tags:emit_tag(label, Text, [
        {class, [label, Record#label.class]},
        {style, Record#label.style}
    ]).
