% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_upload).
-include ("wf.inc").
-compile(export_all).

reflect() -> record_info(fields, upload).

% TODO 
% - Figure out how to access the parent document.
% - Issue a postback on the parent document with the identifier of the file.
% - Handle the postback and call an event on the page module.
% - Does document.parentNode always work in an iframe?

render(ControlID, Record) ->
	FormID = wf:temp_id(),
	IFrameID = wf:temp_id(),
	SubmitJS = wf:f("Nitrogen.$upload(obj('~s'));", [FormID]),
	% wf:wire(ControlID, #event { type=change, actions=SubmitJS }),

	PostbackInfo = action_event:make_postback_info(Record#upload.tag, upload, ControlID, ControlID, ?MODULE),

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
		
		wf:render(#button { text="Upload", actions=#event { type=click, actions=SubmitJS } })
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
			{style, "width: 300px; height: 100px;"}
		])
	].
	
event(Tag) -> 
	?PRINT(Tag),
	ok.