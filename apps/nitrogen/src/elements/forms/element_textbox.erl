% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_textbox).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, textbox).

render_element(Record) -> 
    ID = Record#textbox.id,
    Anchor = Record#textbox.anchor,
    case Record#textbox.next of
        undefined -> ignore;
        Next -> 
            Next1 = wf_render_actions:normalize_path(Next),
            wf:wire(Anchor, #event { type=enterkey, actions=wf:f("Nitrogen.$go_next('~s');", [Next1]) })
    end,

    case Record#textbox.postback of
        undefined -> ignore;
        Postback -> wf:wire(Anchor, #event { type=enterkey, postback=Postback, validation_group=ID, delegate=Record#textbox.delegate })
    end,

    Value = wf:html_encode(Record#textbox.text, Record#textbox.html_encode),
    wf_tags:emit_tag(input, [
        {type, text}, 
        {class, [textbox, Record#textbox.class]},
        {style, Record#textbox.style},
        {value, Value}
    ]).
