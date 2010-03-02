% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_dropdown).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, dropdown).

render_element(Record) -> 
    ID = Record#dropdown.id,
    Anchor = Record#dropdown.anchor,
    case Record#dropdown.postback of
        undefined -> ignore;
        Postback -> wf:wire(Anchor, #event { type=change, postback=Postback, validation_group=ID, delegate=Record#dropdown.delegate })
    end,

    case Record#dropdown.value of 
        undefined -> ok;
        Value -> wf:set(Anchor, Value)
    end,

    Options=case Record#dropdown.options of
        undefined -> "";
        L -> [create_option(X, Record#dropdown.html_encode) || X <- L]
    end,

    wf_tags:emit_tag(select, Options, [
        {class, [dropdown, Record#dropdown.class]},
        {style, Record#dropdown.style}
    ]).

create_option(X, HtmlEncode) ->
    SelectedOrNot = case X#option.selected of
        true -> selected;
        _ -> not_selected
    end,

    Content = wf:html_encode(X#option.text, HtmlEncode),
    Value = wf:html_encode(X#option.value, HtmlEncode),
    wf_tags:emit_tag(option, Content, [
        {value, Value},
        {SelectedOrNot, true}
    ]).
