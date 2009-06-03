% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (pickle_handler).
-export ([
	behaviour_info/1,
	pickle/2,
	depickle/2
]).

behaviour_info(callbacks) -> [
	% init(Context) -> {ok, NewContext, NewState}.
	% Called at the start of the request.
	{init, 1},      

	% finish(Context, State) -> {ok, NewContext, NewState}.
	% Called at the end of the request, before sending
	% a response back to the browser.
	{finish, 2},

	% pickle(Data, Context, State) -> {ok, PickledData, NewContext, NewState}.
	% Turn Data, which could be any valid erlang term, into something that only
	% uses 0-9, A-Z, and a-z.
	{pickle, 3},

	% depickle(PickledData, Context, State) -> {ok, Data, NewContext, NewState}.
	% depickle(PickledData, Context, State) -> {ok, undefined, NewContext, NewState}.
	% Turn pickled data back into raw data. Undefined if it could not be depickled.
	{depickle, 3}
];

behaviour_info(_) -> undefined.

pickle(Data, Context) ->
	wf_context:apply(pickle, pickle, [Data], Context).

depickle(PickledData, Context) ->
	wf_context:apply(pickle, depickle, [PickledData], Context).
