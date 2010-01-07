% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_upload).
-include ("wf.inc").
-include ("simplebridge.hrl").
-compile(export_all).

reflect() -> record_info(fields, upload).

render_element(Record) ->
	ShowButton = Record#upload.show_button,
	ButtonText = Record#upload.button_text,
	Tag = {upload_finished, Record},
	FormID = wf:temp_id(),
	IFrameID = wf:temp_id(),
	SubmitJS = wf:f("Nitrogen.$upload(obj('~s'));", [FormID]),
	PostbackInfo = wf_event:serialize_event_context(Tag, Record#upload.id, undefined, ?MODULE),
	
	% If the button is invisible, then start uploading when the user selects a file.
	wf:wire(Record#upload.id, #event { show_if=(not ShowButton), type=change, actions=SubmitJS }),
	
	% Render the controls and hidden iframe...
	FormContent = [
		wf_tags:emit_tag(input, [
			% {id, HtmlID},
			% {name, HtmlID},
			{class, no_postback},
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
		
		#button { show_if=ShowButton, text=ButtonText, actions=#event { type=click, actions=SubmitJS } }
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
	ValidationGroup = wf_context:event_validation_group(),
	Postback = wf_event:generate_postback_script(NewTag, ValidationGroup, ?MODULE),
	
	% Set the response...
	wf_context:data([
		"<html><body><script>",
		"var Nitrogen = window.parent.Nitrogen;",
		Postback,
		"</script></body></html>"
	]);

event({upload_event, Record, OriginalName, TempFile}) ->
	Module = wf:coalesce([Record#upload.delegate, wf:page_module()]),
	Module:upload_event(Record#upload.tag, OriginalName, TempFile).