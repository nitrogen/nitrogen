% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_upload).
-include ("wf.inc").
-include ("simplebridge.hrl").
-compile(export_all).

reflect() -> record_info(fields, upload).

render_element(HtmlID, Record) ->
	ShowButton = Record#upload.show_button,
	ButtonText = Record#upload.button_text,
	Tag = {upload_finished, Record},
	FormID = wf:temp_id(),
	IFrameID = wf:temp_id(),
	SubmitJS = wf:f("Nitrogen.$upload(obj('~s'));", [FormID]),
	PostbackInfo = wf_event:serialize_event_context(Tag, upload, Record#upload.id, Record#upload.id, ?MODULE),
	
	% If the button is invisible, then start uploading when the user selects a file.
	wf:wire(Record#upload.id, #event { show_if=(not ShowButton), type=change, actions=SubmitJS }),
	
	% Render the controls and hidden iframe...
	FormContent = [
		wf_tags:emit_tag(input, [
			{id, HtmlID},
			{name, HtmlID},
			{type, file}
		]),	
	
		wf_tags:emit_tag(input, [
			{name, eventContext},
			{type, hidden},
			{value, PostbackInfo}
		]),

		wf_tags:emit_tag(input, [
			{name, pageContext},
			{type, hidden},
			{value, ""}
		]),
		
		wf_tags:emit_tag(input, [
			{name, domPaths},
			{type, hidden},
			{value, ""}
		]),
		
		#button { show_if=ShowButton, text=ButtonText, actions=#event { type=click, actions=SubmitJS } }
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
	
event({upload_finished, Record}) ->
	wf_context:type(first_request),
	Req = wf_context:request_bridge(),

	% % Create the postback...
	NewTag = case Req:uploaded_file() of
		undefined -> 
			{upload_event, Record, undefined, undefined};
		#uploaded_file { original_name=OriginalName, temp_file=TempFile } ->
			{upload_event, Record, OriginalName, TempFile}
	end,

	% Make the tag...
	Trigger = wf_context:event_trigger(),
	Target = wf_context:event_target(),
	Postback = wf_event:generate_postback_script(NewTag, upload_finished, Trigger, Target, ?MODULE),

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