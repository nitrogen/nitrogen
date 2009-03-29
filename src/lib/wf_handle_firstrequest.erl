% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_handle_firstrequest).
-include ("wf.inc").
-export ([handle_request/1]).

handle_request(Module) ->
	% Set the initial path...
	case wf:q(object_id) of
		[ObjectID] ->
			put(current_id, ObjectID),
			put(current_path, [ObjectID, page]);
 		_ ->
			put(current_id, "page"),
			put(current_path, [page])
	end,

	% Run the request and get the response body...
	Body = run_module_main(Module),
	
	% Finish by setting the response body, and sending the response.
	wf_platform:set_response_body(Body),
	wf_platform:build_response().


run_module_main(Module) ->
	% Execute the Module, or call web_error if we see an error.
	Body = try
		Module:main()
	catch Type : Msg -> 
		?LOG("ERROR: ~p~n~p~n~p", [Type, Msg, erlang:get_stacktrace()]),
		web_error:main()
	end,

	% Call render if it has not already been called.
	case wf:is_string(Body) of
		true -> Body;
		false -> wf:render(Body)
	end.

