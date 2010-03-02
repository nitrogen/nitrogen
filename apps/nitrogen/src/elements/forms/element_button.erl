% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_button).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, button).

render_element(Record) ->
    ID = Record#button.id,
    Anchor = Record#button.anchor,
    case Record#button.postback of
        undefined -> ignore;
        Postback -> wf:wire(Anchor, #event { type=click, validation_group=ID, postback=Postback, delegate=Record#button.delegate })
    end,

    Value = ["  ", wf:html_encode(Record#button.text, Record#button.html_encode), "  "], 
    wf_tags:emit_tag(input, [
        {type, button},
        {class, [button, Record#button.class]},
        {style, Record#button.style},
        {value, Value}
    ]).
