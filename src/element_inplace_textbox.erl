% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_inplace_textbox).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, inplace_textbox).

render(ControlID, Record) -> 
	% Get vars...
	OKButtonID = wf:temp_id(),
	CancelButtonID = wf:temp_id(),
	ViewPanelID = wf:temp_id(),
	EditPanelID = wf:temp_id(),
	LabelID = wf:temp_id(),
	MouseOverID = wf:temp_id(),
	TextBoxID = wf:temp_id(),
	Delegate = Record#inplace_textbox.delegate,
	Tag = Record#inplace_textbox.tag,
	OriginalText = Record#inplace_textbox.text,
	
	% Set up the events...
	Controls = {ViewPanelID, LabelID, EditPanelID, TextBoxID},
	OKEvent = #event { delegate=?MODULE, postback={ok, Controls, Delegate, Tag}, controls=[TextBoxID] },
	CancelEvent = #event { delegate=?MODULE, postback={cancel, Controls, Delegate, Tag, OriginalText} },
	
	% Create the view...
	Text = Record#inplace_textbox.text,
	Terms = #panel { 
		class="inplace_textbox " ++ wf:to_list(Record#inplace_textbox.class),
		style=Record#inplace_textbox.style,
		body = [
			#panel { id=ViewPanelID, class="view", body=[
				#span { id=LabelID, class="label", text=Text, html_encode=Record#inplace_textbox.html_encode, actions=[
					#buttonize { target=ViewPanelID },
					#event { type=mouseover, target=MouseOverID, actions=#effect_show {} },
					#event { type=mouseout, target=MouseOverID, actions=#effect_hide{} }
				]},
				#span { id=MouseOverID, class="instructions", text="Click to edit" }
			], actions = [
				#event { type=click, actions=wf:f("obj('~s').hide(); obj('~s').show(); obj('~s').focus(); obj('~s').select();", [ViewPanelID, EditPanelID, TextBoxID, TextBoxID]) }
			]},
			#panel { id=EditPanelID, class="edit", body=[
				#textbox { id=TextBoxID, text=Text, next=OKButtonID },
				#button { id=OKButtonID, text="OK", actions=OKEvent#event { type=click } },
				#button { id=CancelButtonID, text="Cancel", actions=CancelEvent#event { type=click } }
			]}
		]
	},
	
	case Record#inplace_textbox.start_mode of
		view -> wf:wire(EditPanelID, #effect_hide{});
		edit -> 
			wf:wire(ViewPanelID, #effect_hide{}),
			wf:wire(TextBoxID, "obj('me').focus(); obj('me').select();")
	end,
	
	wf:wire(OKButtonID, TextBoxID, #validate { attach_to=CancelButtonID, validators=Record#inplace_textbox.validators }),
	
	element_panel:render(ControlID, Terms).

event({ok, {ViewPanelID, LabelID, EditPanelID, TextBoxID}, Delegate, Tag}) -> 
	[Value] = wf:q(TextBoxID),
	Value1 = Delegate:inplace_textbox_event(Tag, Value),
	wf:update(LabelID, Value1),
	% Safari bitches with the following line, so 
	% update the textbox through javascript instead.
	%wf:update(TextBoxID, Value1),
	wf:wire(wf:f("obj('~s').value = \"~s\";", [TextBoxID, wf_utils:js_escape(Value1)])),
	wf:wire(wf:f("obj('~s').hide(); obj('~s').show();", [EditPanelID, ViewPanelID])),
	ok;

event({cancel, {ViewPanelID, _LabelID, EditPanelID, TextBoxID}, _Delegate, _Tag, OriginalText}) ->
	%wf:update(TextBoxID, OriginalText),
	wf:wire(wf:f("obj('~s').value = \"~s\";", [TextBoxID, OriginalText])),
	wf:wire(wf:f("obj('~s').hide(); obj('~s').show();", [EditPanelID, ViewPanelID])),
	ok;	

event(_Tag) -> ok.