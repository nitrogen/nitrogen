% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_handle).
-include ("wf.inc").
-export ([handle_request/1]).

handle_request(Module) ->
	% Set up session.
	L = [wf_action_queue, wf_update_queue, wf_content_script, wf_dom_script, wf_script, wf_paths, wf_state, wf_state_updates, wf_headers],
	[put(X, []) || X <- L],
	wf_platform:clear_redirect(),
	wf_platform:set_response_code(200),
	wf_platform:set_content_type("text/html"),
	wf_platform:set_response_body([]),
	wf_session:ensure_session(),
	wf_path:register_path(page, [page]),
	
	% Check authorization...
	case wf_global:request(Module) of
		ok ->
			% Process the request...
			case wf_platform:request_method() of 
				'GET' -> 
					try handle_get_request(Module) 
					catch Type : Msg -> 
						?LOG("ERROR: ~p~n~p~n~p", [Type, Msg, erlang:get_stacktrace()]),
						handle_get_request(web_error) 
					end;
				'POST' -> 
					try handle_post_request(Module) 
					catch Type : Msg -> 
						?LOG("ERROR: ~p~n~p~n~p", [Type, Msg, erlang:get_stacktrace()]),
						wf_platform:set_content_type("application/javascript"),
						wf_platform:set_response_body("alert('An error has occurred. Please refresh this page and try again.');"),
						wf_platform:build_response()
					end
			end;
		Response -> 
			% Something else happened, so return the given response.
			wf_platform:set_response_body(Response),
			wf_platform:build_response()
	end.
	
handle_get_request(Module) ->
	% Process the query...
	Query = wf_platform:parse_get_args(),
	wf_query:prepare_request_query_paths(Query),
	
	% Render the page...
	wf:state(validators, []),
	put(current_path, [page]),	
	Response = Module:main(),
	
	% Send the response.
	wf_platform:set_response_body(Response),	
	wf_platform:build_response().


handle_post_request(Module) ->
	% Process the query...
	Query = wf_platform:parse_post_args(),
	put(request_query, Query),
	wf_path:restore_paths_from_post(Query),
	wf_state:restore_state_from_post(Query),
	wf_query:prepare_request_query_paths(Query),
	
	% Get the event that triggered the postback...
	[EventInfo] = wf:q(eventInfo),
	{EventType, TriggerID, TargetID, Tag, Delegate} = wf:depickle(EventInfo),
	
	% Move to the right path...
	put(current_path, wf_path:to_path(TargetID)),
	
	% Figure out if we should use a delegate.
	Module1 = case Delegate of 
		undefined -> Module;
		_ -> Delegate
	end,
	
	case EventType of
		continuation ->
			% If it's a continuation, then see if it is still running or finished.
			% If it is still running, then re-register the callback.
			% If it is finished, then call continue with the result.
			Pid = Tag,
			case wf_continuation:get_result(Pid) of
				{running, Interval} -> wf_continuation:register(Pid, Interval);
				{done, InnerTag, Result} -> Module1:continue(InnerTag, Result)
			end;
			
		_ ->
			% If it's a standard event, then validate based on the trigger.
			% If validation is successful, then call the event.
			case wf_validation:validate(TriggerID) of
				true -> Module1:event(Tag);
				false -> ok
			end
	end,
	
	% Process any 'flash' messages so long as:
	% - A flash element is present.
	% - This isn't an event FROM the flash element.
	% - We are not redirecting.
	HasFlash = wf_path:to_paths(flash) /= [],
	case HasFlash andalso Module1 /= element_flash andalso get(is_redirect) /= true of
		true -> element_flash:update();
		false -> ok
	end,
	
	% Move to the right path...
	put(current_path, wf_path:to_path(TargetID)),
	wf_platform:set_content_type("application/javascript"),
	wf_platform:set_response_body(wf_script:get_script()),
	wf_platform:build_response().