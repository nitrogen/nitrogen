% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_upload).
-include ("wf.inc").
-include ("simplebridge.hrl").
-compile(export_all).

reflect() -> record_info(fields, upload).

render_element(Record) ->
	Anchor = Record#upload.anchor,
	ShowButton = Record#upload.show_button,
	ButtonText = Record#upload.button_text,
	StartedTag = {upload_started, Record},
	FinishedTag = {upload_finished, Record}, 
	FormID = wf:temp_id(),
	IFrameID = wf:temp_id(),
	ButtonID = wf:temp_id(),
	SubmitJS = wf:f("Nitrogen.$upload(jQuery('#~s').get(0));", [FormID]),
	PostbackInfo = wf_event:serialize_event_context(FinishedTag, Record#upload.id, undefined, ?MODULE),
	
	% Create a postback that is called when the user first starts the upload...
	wf:wire(Anchor, #event { show_if=(not ShowButton), type=change, delegate=?MODULE, actions=postback=StartedTag }),
	wf:wire(ButtonID, #event { show_if=ShowButton, type=click, delegate=?MODULE, postback=StartedTag }),

	% If the button is invisible, then start uploading when the user selects a file.
	wf:wire(Anchor, #event { show_if=(not ShowButton), type=change, actions=SubmitJS }),
	wf:wire(ButtonID, #event { show_if=ShowButton, type=click, actions=SubmitJS }),
		
	% Render the controls and hidden iframe...
	FormContent = [
		wf_tags:emit_tag(input, [
			{name, file},
			{class, [no_postback|Anchor]},
			{type, file}
		]),	
	
		wf_tags:emit_tag(input, [
			{name, eventContext},
			{type, hidden},
			{class, no_postback},
			{value, PostbackInfo}
		]),

		wf_tags:emit_tag(input, [
			{name, pageContext},
			{type, hidden},
			{class, no_postback},
			{value, ""}
		]),
		
		wf_tags:emit_tag(input, [
			{type, hidden},
			{class, no_postback},
			{value, ""}
		]),
		
		#button { id=ButtonID, show_if=ShowButton, text=ButtonText }
	],
	
	[
		wf_tags:emit_tag(form, FormContent, [
			{id, FormID},
			{name, upload}, 
			{method, 'POST'},
			{enctype, "multipart/form-data"},
			{class, no_postback},
			{target, IFrameID}
		]),
		
		wf_tags:emit_tag(iframe, [], [
			{id, IFrameID},
			{name, IFrameID},
			{style, "display: none; width: 300px; height: 100px;"}
		])
	].
	
% This event is fired when the user first clicks the upload button.
event({upload_started, Record}) ->
	Module = wf:coalesce([Record#upload.delegate, wf:page_module()]),
	Module:start_upload_event(Record#upload.tag);
    
	
% This event is called once the upload post happens behind the scenes.
% It happens somewhat outside of Nitrogen, so the next thing we do
% is trigger a postback that happens inside of Nitrogen. 
event({upload_finished, Record}) ->
	wf_context:type(first_request),
	Req = wf_context:request_bridge(),
	
	% % Create the postback...
	NewTag = case Req:post_files() of
		[] -> 
			{upload_event, Record, undefined, undefined};
		[#uploaded_file { original_name=OriginalName, temp_file=TempFile }|_] ->
			{upload_event, Record, OriginalName, TempFile}
	end,
	
	% Make the tag...
	Anchor = wf_context:anchor(),
	ValidationGroup = wf_context:event_validation_group(),
	Postback = wf_event:generate_postback_script(NewTag, Anchor, ValidationGroup, ?MODULE, undefined),
	
	% Set the response...
	wf_context:data([
		"<html><body><script>",
		"var Nitrogen = window.parent.Nitrogen;",
		Postback,
		"</script></body></html>"
	]);

% This event is fired by the upload_finished event, it calls
% back to the page or control that contained the upload element.
event({upload_event, Record, OriginalName, TempFile}) ->
	Module = wf:coalesce([Record#upload.delegate, wf:page_module()]),
	Module:finish_upload_event(Record#upload.tag, OriginalName, TempFile).