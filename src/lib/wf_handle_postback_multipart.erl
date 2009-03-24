% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_handle_postback_multipart).
-include ("wf.inc").
-export ([handle_request/1]).

handle_request(Module) ->
	Request = wf_platform:get_request(),
	?PRINT(mochiweb_multipart:parse_multipart_request(Request, fun callback/1)),
	wf_platform:set_response_body("Done"),
	wf_platform:build_response().

callback(X) -> 
	?PRINT(X), 
	F = fun(Y) -> callback(Y) end,
	F.

	% % Get the event that triggered the postback...
	% [PostbackInfo] = wf:q(postbackInfo),
	% {ObjectID, Tag, _EventType, TriggerID, TargetID, Delegate} = wf:depickle(PostbackInfo),
	% 
	% % Find the delegate...
	% Module1 = case Delegate of 
	% 	undefined -> Module;
	% 	_ -> Delegate
	% end,	
	% 
	% % Get the filename...
	% File = case wf:q(TriggerID) of
	% 	[X] -> X;
	% 	_   -> undefined
	% end,
	% 
	% % Create the postback...
	% NewTag = {upload, Tag, File},
	% Postback = action_event:make_postback(NewTag, upload_finished, TriggerID, TargetID, Module1),
	% 
	% % Send the response...
	% wf_platform:set_content_type("text/html"),
	% wf_platform:set_response_body([
	% 	"<html><body><script>",
	% 	wf:f("window.parent.~s.", [ObjectID]), Postback,
	% 	"</script></body></html>"
	% ]),
	% wf_platform:build_response().