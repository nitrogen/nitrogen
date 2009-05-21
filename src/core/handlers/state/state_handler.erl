% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (state_handler).
-export ([
	behaviour_info/1
]).

behaviour_info(callbacks) -> [
	% init(Context) -> {ok, NewContext, NewState}.
	% Called at the start of the request.
	{init, 1},      

	% finish(Context, State) -> {ok, NewContext, NewState}.
	% Called at the end of the request, before sending
	% a response back to the browser.
	{finish, 2},
	
	% get(Context, State, Key, DefaultValue) -> {ok, NewContext, NewState, Value}.
	% Retrieve a value from the storage area.
	{get, 4},       
	
	% put(Context, State, Key, Value) -> {ok, NewContext, NewState}.
	% Put a value into the storage area.
	{put, 4},
	
	% clear(Context, State, Key) -> {ok, NewContext, NewState}.
	% Remove a value from the storage area.
	{clear, 3},
	
	% clear_all(Context, State) -> {ok, NewContext, NewState}.
	% Clear all values from the storage area.
	{clear_all, 2}
];

behaviour_info(_) -> undefined.
