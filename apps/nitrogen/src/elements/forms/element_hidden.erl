% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_hidden).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, hidden).

render_element(Record) -> 
    Value = wf:html_encode(Record#hidden.text, Record#hidden.html_encode),
    wf_tags:emit_tag(input, [
        {class, Record#hidden.class},
        {type, hidden},
        {value, Value}
    ]).
