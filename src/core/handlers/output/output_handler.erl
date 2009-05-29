% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (output_handler).
-export ([
	behaviour_info/1,
	build_response/3
]).

behaviour_info(callbacks) -> [
	% init(Context) -> {ok, NewContext, NewState}.
	% Called at the start of the request.
	{init, 1},      

	% finish(Context, State) -> {ok, NewContext, NewState}.
	% Called at the end of the request, before sending
	% a response back to the browser.
	{finish, 2},
	
	% build_response(HTML, Script, Context, State) -> {ok, NewContext, NewState}.
	% Accepts HTML and Javascript, loads it into the ResponseBridge (parameterized
	% module from the SimpleBridge project), and calls Bridge:build_response(), which
	% sends a response appropriate to whatever web server is being used.
	{build_response, 4}
];

behaviour_info(_) -> undefined.

build_response(Html, Script, Context) ->
	wf_context:apply_return_raw(output, build_response, [Html, Script], Context).
