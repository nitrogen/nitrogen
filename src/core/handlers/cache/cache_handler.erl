% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (cache_handler).
-export ([
	behaviour_info/1
]).

behaviour_info(callbacks) -> [
	% init(Context) -> {ok, NewContext, NewState}
	% Called at the start of the request.
	{init, 1},      

	% finish(Context, State) -> {ok, NewContext, NewState}
	% Called at the end of the request, before sending
	% a response back to the browser.
	{finish, 2},
	
	% get_set(Context, State, Key, Function, TTL) -> {ok, NewContext, NewState, Value}
	% Return the cache value associated with Key. If it is not found,
	% then run the Function, store the resulting value in cache under
	% Key, and return the value.
	{get_set, 5}, 
	
	% clear(Context, State, Key) -> {ok, NewContext, NewState}
	% Remove a value from cache.
	{clear, 3},
	
	% clear_all(Context, State) -> {ok, NewContext, NewState}
	% Clear all values from cache.
	{clear_all, 2}
];

behaviour_info(_) -> undefined.
