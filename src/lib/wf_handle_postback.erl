% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_handle_postback).
-include ("wf.inc").
-export ([handle_request/1]).

handle_request(Module) ->
	% Restore Nitrogen state...
	wf_state:restore_state(),
	
	% Get the event that triggered the postback...
	[PostbackInfo] = wf:q(postbackInfo),
	{ObjectID, Tag, EventType, TriggerID, TargetID, Delegate} = wf:depickle(PostbackInfo),

	% Figure out if we should use a delegate.
	Module1 = case Delegate of 
		undefined -> Module;
		_ -> Delegate
	end,
	
	% Setup the response...
	wf_platform:set_content_type("application/javascript"),
	put(current_id, ObjectID),
	put(current_path, wf_path:to_path(TargetID)),
	
	% Do the event...
	case EventType of
		comet -> handle_comet_request();
		continuation -> handle_continuation_request(Module1, Tag);
		_ -> handle_normal_request(Module1, TriggerID, Tag)
	end.



%%% COMET REQUEST %%%

handle_comet_request() -> 
	Content = wf_comet:get_content(),
	wf_platform:set_response_body(Content),
	wf_platform:build_response().



%%% CONTINUATION REQUEST %%%

handle_continuation_request(Module, Tag) ->
	% Check if the continuation is still running, or if it finished.
	% If it is still running, then re-register the callback.
	% If it is finished, then call continue with the result.
	Pid = Tag,
	case wf_continuation:get_result(Pid) of
		{running, Interval} -> wf_continuation:register(Pid, Interval);
		{done, InnerTag, Result} -> run_module_continue(Module, InnerTag, Result)
	end,
	
	% Make sure to update the flash module, if it exists.
	element_flash:update(Module),
	
	% Assemble all Javascript, and send to the browser.
	Script = wf_script:get_script(),
	wf_platform:set_response_body(Script),
	wf_platform:build_response().
	
run_module_continue(Module, InnerTag, Result) ->
	try
		Module:continue(InnerTag, Result)
	catch Type : Msg -> 
		?LOG("ERROR: ~p~n~p~n~p", [Type, Msg, erlang:get_stacktrace()]),
		wf:wire("alert('An error has occurred. Please refresh this page and try again.');")
	end.



%%% NORMAL REQUEST %%%

handle_normal_request(Module, TriggerID, Tag) ->	
	% Validate based on the trigger.
	% If validation is successful, then call the event.
	case wf_validation:validate(TriggerID) of
		true -> run_module_event(Module, Tag);
		false -> ok
	end,

	% Make sure to update the flash module, if it exists.
	element_flash:update(Module),
	
	% Assemble all Javascript, and send to the browser.
	Script = wf_script:get_script(),
	wf_platform:set_response_body(Script),
	wf_platform:build_response().
	
run_module_event(Module, Tag) ->
	try 
		Module:event(Tag)
	catch Type : Msg -> 
		?LOG("ERROR: ~p~n~p~n~p", [Type, Msg, erlang:get_stacktrace()]),
		wf:wire("alert('An error has occurred. Please refresh this page and try again.');")
	end.