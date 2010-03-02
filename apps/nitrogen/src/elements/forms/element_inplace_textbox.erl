% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_inplace_textbox).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, inplace_textbox).

render_element(Record) -> 
    % Get vars...
    OKButtonID = wf:temp_id(),
    CancelButtonID = wf:temp_id(),
    ViewPanelID = wf:temp_id(),
    EditPanelID = wf:temp_id(),
    LabelID = wf:temp_id(),
    MouseOverID = wf:temp_id(),
    TextBoxID = wf:temp_id(),
    Tag = Record#inplace_textbox.tag,
    OriginalText = Record#inplace_textbox.text,
    Delegate = Record#inplace_textbox.delegate,

    % Set up the events...
    Controls = {ViewPanelID, LabelID, EditPanelID, TextBoxID},
    OKEvent = #event { delegate=?MODULE, postback={ok, Delegate, Controls, Tag} },
    CancelEvent = #event { delegate=?MODULE, postback={cancel, Controls, Tag, OriginalText} },

    % Create the view...
    Text = Record#inplace_textbox.text,
    Terms = #panel { 
        class=[inplace_textbox, Record#inplace_textbox.class],
        style=Record#inplace_textbox.style,
        body = [
            #panel { id=ViewPanelID, class="view", body=[
                #span { id=LabelID, class="label", text=Text, html_encode=Record#inplace_textbox.html_encode, actions=[
                    #buttonize { target=ViewPanelID }
                ]},
                #span { id=MouseOverID, class="instructions", text="Click to edit", actions=#hide{} }
            ], actions = [
                    #event { type=click, actions=[
                        #hide { target=ViewPanelID },
                        #show { target=EditPanelID },
                        #script { script = wf:f("obj('~s').focus(); obj('~s').select();", [TextBoxID, TextBoxID]) }
                    ]},
                    #event { type=mouseover, target=MouseOverID, actions=#show{} },
                    #event { type=mouseout, target=MouseOverID, actions=#hide{} }
            ]},
            #panel { id=EditPanelID, class="edit", body=[
                #textbox { id=TextBoxID, text=Text, next=OKButtonID },
                #button { id=OKButtonID, text="OK", actions=OKEvent#event { type=click } },
                #button { id=CancelButtonID, text="Cancel", actions=CancelEvent#event { type=click } }
            ]}
        ]
    },

    case Record#inplace_textbox.start_mode of
        view -> wf:wire(EditPanelID, #hide{});
        edit -> 
            wf:wire(ViewPanelID, #hide{}),
            Script = #script { script="obj('me').focus(); obj('me').select();" },
            wf:wire(TextBoxID, Script)
    end,

    wf:wire(OKButtonID, TextBoxID, #validate { attach_to=CancelButtonID, validators=Record#inplace_textbox.validators }),

    element_panel:render_element(Terms).

event({ok, Delegate, {ViewPanelID, LabelID, EditPanelID, TextBoxID}, Tag}) -> 
    Value = wf:q(TextBoxID),
    Module = wf:coalesce([Delegate, wf:page_module()]),
    Value1 = Module:inplace_textbox_event(Tag, Value),
    wf:update(LabelID, Value1),
    wf:set(TextBoxID, Value1),
    wf:wire(EditPanelID, #hide {}),
    wf:wire(ViewPanelID, #show {}),
    ok;

event({cancel, {ViewPanelID, _LabelID, EditPanelID, TextBoxID}, _Tag, OriginalText}) ->
    wf:set(TextBoxID, OriginalText),
    wf:wire(EditPanelID, #hide {}),
    wf:wire(ViewPanelID, #show {}),
    ok;

event(_Tag) -> ok.
