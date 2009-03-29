% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_handle).
-include ("wf.inc").
-export ([handle_request/2]).

handle_request(Module, PathInfo) ->
	reset_response(Module, PathInfo),
	check_request_authorization(Module).

check_request_authorization(Module) ->
	case wf_platform:request(Module) of
		ok ->
			check_multipart_postback(Module);
		Response -> 
			% Something else happened, so return the given response.
			wf_platform:set_response_body(Response),
			wf_platform:build_response()
	end.
	
check_multipart_postback(Module) ->
	case wf_platform:get_header(content_type) of
		"multipart/form-data" ++ _ ->
			wf_handle_postback_multipart:handle_request(Module);
		_ -> 
			check_request_method(Module)
	end.

check_request_method(Module) ->
	% Get the query...
	Query = case wf_platform:get_request_method() of
		'GET' -> wf_platform:parse_get_args();
		'POST' -> wf_platform:parse_post_args()
	end,
	put(request_query, Query),
	wf_query:prepare_request_query_paths(Query),

	% Check if this is the first request or a postback...
	IsFirstRequest = wf_platform:get_request_method() == 'GET' andalso wf:q(postbackInfo) == [],
	case IsFirstRequest of 
		true  -> wf_handle_firstrequest:handle_request(Module);
		false -> wf_handle_postback:handle_request(Module) 
	end.
			
reset_response(Module, PathInfo) ->
	% Set up process dictionary...
	L = [wf_action_queue, wf_update_queue, wf_content_script, wf_script, wf_paths, wf_state, wf_headers],
	[put(X, []) || X <- L],
	
	% Response defaults...
	wf:state(validators, []),
	wf_platform:set_page_module(Module),
	wf_platform:set_path_info(PathInfo),
	wf_platform:clear_redirect(),
	wf_platform:set_response_code(200),
	wf_platform:set_content_type("text/html"),
	wf_platform:set_response_body([]),
	
	% Create the session if it doesn't already exist...
	wf_session:ensure_session().

	


