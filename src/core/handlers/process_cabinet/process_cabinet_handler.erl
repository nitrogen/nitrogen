% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (process_cabinet_handler).
-export ([
	behaviour_info/1,
	get/2,
	get_set/3
]).

behaviour_info(callbacks) -> [
	% init(Context) -> {ok, NewContext, NewState}.
	% Called at the start of the request.
	{init, 1},      

	% finish(Context, State) -> {ok, NewContext, NewState}.
	% Called at the end of the request, before sending
	% a response back to the browser.
	{finish, 2},
	
	% get(Key, Context, State) -> {ok, Pid, NewContext, NewState}.
	% Get the process associated with this Key.
	{get, 3},
	
	% get_set(Key, Function, Context, State) -> {ok, Pid, NewContext, NewState}.	
	% Return the process associated with Key. If that process does not
	% exist, then create a new process and associate it with Key.
	{get_set, 4}
];

behaviour_info(_) -> undefined.

get(Key, Context) ->
	wf_context:apply(process_cabinet, get, [Key], Context).

get_set(Key, Function, Context) ->
	wf_context:apply(process_cabinet, get_set, [Key, Function], Context).
		