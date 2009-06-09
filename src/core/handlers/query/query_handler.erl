% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (query_handler).
-export ([
	behaviour_info/1,
	get_value/2
]).

behaviour_info(callbacks) -> [
	% init(Context) -> {ok, NewContext, NewState}.
	% Called at the start of the request.
	{init, 1},      

	% finish(Context, State) -> {ok, NewContext, NewState}.
	% Called at the end of the request, before sending
	% a response back to the browser.
	{finish, 2},
	
	% get_value(Path, Context, State) -> Value.
	% Given a path, return the parameter value.
	{get_value, 3}
];

behaviour_info(_) -> undefined.

get_value(Path, Context) ->
	wf_context:apply_return_raw('query', get_value, [Path], Context).