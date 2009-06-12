% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_upload).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, upload).

render(ControlID, Record) ->
	ShowButton = Record#upload.show_button,
	ButtonText = Record#upload.button_text,
	FormID = wf:temp_id(),
	IFrameID = wf:temp_id(),
	SubmitJS = wf:f("Nitrogen.$upload(obj('~s'));", [FormID]),
	PostbackInfo = action_event:make_postback_info(Record#upload.tag, upload, ControlID, ControlID, ?MODULE),
	
	% If the button is invisible, then start uploading when the user selects a file.
	wf:wire(ControlID, #event { show_if=(not ShowButton), type=change, actions=SubmitJS }),
	
	% Render the controls and hidden iframe...
	FormContent = [
		wf_tags:emit_tag(input, [
			{id, ControlID},
			{name, ControlID},
			{type, file}
		]),	
	
		wf_tags:emit_tag(input, [
			{name, postbackInfo},
			{type, hidden},
			{value, PostbackInfo}
		]),
		
		wf_tags:emit_tag(input, [
			{name, domState},
			{type, hidden},
			{value, ""}
		]),
		
		wf:render(#button { show_if=ShowButton, text=ButtonText, actions=#event { type=click, actions=SubmitJS } })
	],
	
	[
		wf_tags:emit_tag(form, FormContent, [
			{id, FormID},
			{name, FormID}, 
			{method, 'POST'},
			{enctype, "multipart/form-data"},
			{target, IFrameID}
		]),
		
		wf_tags:emit_tag(iframe, [], [
			{id, IFrameID},
			{name, IFrameID},
			{style, "display: none; width: 300px; height: 100px;"}
		])
	].
	
event({upload, Tag, Filename, LocalFileData}) -> 
	Delegate = wf_platform:get_page_module(),
	Delegate:upload_event(Tag, Filename, LocalFileData),
	ok.