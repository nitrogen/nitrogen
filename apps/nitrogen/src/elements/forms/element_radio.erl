% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_radio).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, radio).

render_element(Record) -> 
    ID = Record#radio.id,
    Anchor = Record#radio.anchor,
    CheckedOrNot = case Record#radio.checked of
        true -> checked;
        _ -> not_checked
    end,

    case Record#radio.postback of
        undefined -> ignore;
        Postback -> wf:wire(Anchor, #event { type=change, postback=Postback, validation_group=ID, delegate=Record#radio.delegate })
    end,

    Content = wf:html_encode(Record#radio.text, Record#radio.html_encode),

    [
        %% Checkbox...
        wf_tags:emit_tag(input, [
            {name, Record#radio.name},
            {value, Record#radio.value},
            {type, radio},
            {class, [radio, Record#radio.class]},
            {style, Record#radio.style},
            {CheckedOrNot, true}
        ]),

        %% Label for Radio...
        wf_tags:emit_tag(label, Content, [
            {for, Anchor}
        ])
    ].
