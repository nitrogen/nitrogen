% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_handle_postback_multipart).
-include ("wf.inc").
-export ([handle_request/1]).

handle_request(Module) ->
	% Parse the template...
 	{ok, Query, FileName, LocalFileData} = wf_multipart:parse_upload(),
	put(request_query, Query),
	wf_query:prepare_request_query_paths(Query),
	

	% Get the event that triggered the postback...
	[PostbackInfo] = wf:q(postbackInfo),
	{ObjectID, Tag, _EventType, TriggerID, TargetID, Delegate} = wf:depickle(PostbackInfo),
	
	% Find the delegate...
	Module1 = case Delegate of 
		undefined -> Module;
		_ -> Delegate
	end,	
	
	% Setup the response...
	put(current_id, ObjectID),
	put(current_path, wf_path:to_path(TargetID)),

	% Create the postback...
	NewTag = {upload, Tag, FileName, LocalFileData},
	Postback = action_event:make_postback(NewTag, upload_finished, TriggerID, TargetID, Module1),
	
	% Send the response...
	wf_platform:set_content_type("text/html"),
	wf_platform:set_response_body([
		"<html><body><script>",
		"var Nitrogen = window.parent.Nitrogen;",
		wf:me_var(),
		Postback,
		"</script></body></html>"
	]),
	wf_platform:build_response().