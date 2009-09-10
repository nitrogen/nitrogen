% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (cache_handler).
-export ([
	behaviour_info/1, get_cached/3, clear/1, clear_all/0
]).



% get_cached(Key, Function, TTL, State) -> {ok, Value, NewState}
% Return the cache value associated with Key. If it is not found,
% then run the Function, store the resulting value in cache under
% Key, and return the value.
get_cached(Key, Function, TTL) -> 
	{ok, _Value} = wf_handler:call(cache_handler, get_cached, [Key, Function, TTL]).

% clear(Key, State) -> {ok, NewState}
% Remove a value from cache.
clear(Key) ->	
	ok = wf_handler:call(cache_handler, clear, [Key]).
	
% clear_all(State) -> {ok, NewState}
% Clear all values from cache.
clear_all() -> 
	ok = wf_handler:call(cache_handler, clear_all).



behaviour_info(callbacks) -> [
	{init, 1},      
	{finish, 1},
	{get_cached, 4}, 
	{clear, 2},
	{clear_all, 1}
];
behaviour_info(_) -> undefined.
