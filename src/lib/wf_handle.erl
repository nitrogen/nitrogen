% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_handle).
-include ("wf.inc").
-export ([handle_request/2]).

handle_request(Module, PathInfo) ->
	% Set up session.
	L = [wf_action_queue, wf_update_queue, wf_content_script, wf_script, wf_paths, wf_state, wf_headers],
	[put(X, []) || X <- L],
	wf_platform:set_page_module(Module),
	wf_platform:set_path_info(PathInfo),
	wf_platform:clear_redirect(),
	wf_platform:set_response_code(200),
	wf_platform:set_content_type("text/html"),
	wf_platform:set_response_body([]),
	wf_session:ensure_session(),
	
	% Check authorization...
	case wf_platform:request(Module) of
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
	Body1 = Module:main(),

	% Call render if it has not already been called.
	Body2 = case wf:is_string(Body1) of
		true -> Body1;
		false -> wf:render(Body1)
	end,

	wf_platform:set_response_body(Body2),	
	wf_platform:build_response().


handle_post_request(Module) ->
	% Process the query...
	Query = wf_platform:parse_post_args(),
	put(request_query, Query),
	wf_state:restore_state_from_post(Query),
	wf_query:prepare_request_query_paths(Query),
	wf_platform:set_content_type("application/javascript"),
	
	% Get the event that triggered the postback...
	[PostbackInfo] = wf:q(postbackInfo),
	{EventType, TriggerID, TargetID, Tag, Delegate} = wf:depickle(PostbackInfo),
	
	% Figure out if we should use a delegate.
	Module1 = case Delegate of 
		undefined -> Module;
		_ -> Delegate
	end,
	
	% Move to the right path...
	put(current_path, wf_path:to_path(TargetID)),

	case EventType of
		comet ->
			handle_post_request_comet();
			
		continuation -> 
			handle_post_request_continuation(Module1, TriggerID, TargetID, Tag);
			
		_ -> 
			handle_post_request_normal(Module1, TriggerID, TargetID, Tag)
	end.
	
handle_post_request_comet() -> 
	Content = wf_comet:get_content(),
	wf_platform:set_response_body(Content),
	wf_platform:build_response().

handle_post_request_continuation(Module, _TriggerID, TargetID, Tag) ->
	% Move to the right path...
	put(current_path, wf_path:to_path(TargetID)),

	% Check if the continuation is still running, or if it finished.
	% If it is still running, then re-register the callback.
	% If it is finished, then call continue with the result.
	Pid = Tag,
	case wf_continuation:get_result(Pid) of
		{running, Interval} -> wf_continuation:register(Pid, Interval);
		{done, InnerTag, Result} -> Module:continue(InnerTag, Result)
	end,
	
	element_flash:update(Module),
	Script = wf_script:get_script(),
	wf_platform:set_response_body(Script),
	wf_platform:build_response().

handle_post_request_normal(Module, TriggerID, TargetID, Tag) ->	
	% Move to the right path...
	put(current_path, wf_path:to_path(TargetID)),

	% Validate based on the trigger.
	% If validation is successful, then call the event.
	case wf_validation:validate(TriggerID) of
		true -> Module:event(Tag);
		false -> ok
	end,

	element_flash:update(Module),
	Script = wf_script:get_script(),
	wf_platform:set_response_body(Script),
	wf_platform:build_response().
	