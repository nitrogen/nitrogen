% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_inplace_textbox).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, inplace_textbox).

render_element(HtmlID, Record, Context) -> 
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
	
	% Set up the events...
	Controls = {ViewPanelID, LabelID, EditPanelID, TextBoxID},
	OKEvent = #event { delegate=?MODULE, postback={ok, Controls, Tag} },
	CancelEvent = #event { delegate=?MODULE, postback={cancel, Controls, Tag, OriginalText} },
	
	% Create the view...
	Text = Record#inplace_textbox.text,
	Terms = #panel { 
		class="inplace_textbox " ++ wf:to_list(Record#inplace_textbox.class),
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
	
	{ok, Context2} = case Record#inplace_textbox.start_mode of
		view -> wff:wire(EditPanelID, #hide{}, Context);
		edit -> 
			{ok, Context1} = wff:wire(ViewPanelID, #hide{}, Context),
			Script = #script { script="obj('me').focus(); obj('me').select();" },
			{ok, _Context2} = wff:wire(TextBoxID, Script, Context1)
	end,
	
	{ok, Context3} = wff:wire(OKButtonID, TextBoxID, #validate { attach_to=CancelButtonID, validators=Record#inplace_textbox.validators }, Context2),
	
	element_panel:render_element(HtmlID, Terms, Context3).

event({ok, {ViewPanelID, LabelID, EditPanelID, TextBoxID}, Tag}, Context) -> 
	Value = wff:q(TextBoxID, Context),
	Delegate = wff:get_page_module(Context),
	Value1 = Delegate:inplace_textbox_event(Tag, Value),
	wf:update(LabelID, Value1),
	%TODO - wf:set(TextBoxID, Value1),
	{ok, Context1} = wff:wire(EditPanelID, #hide {}, Context),
	{ok, Context2} = wff:wire(ViewPanelID, #show {}, Context1),
	{ok, Context2};

event({cancel, {ViewPanelID, _LabelID, EditPanelID, _TextBoxID}, _Tag, _OriginalText}, Context) ->
	% TODO - wf:set(TextBoxID, OriginalText),
	{ok, Context1} = wff:wire(EditPanelID, #hide {}, Context),
	{ok, Context2} = wff:wire(ViewPanelID, #show {}, Context1),
	{ok, Context2}.

event(_Tag) -> ok.