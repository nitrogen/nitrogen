% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (cache_bridge).
-export ([
	behaviour_info/1
]).

behaviour_info(callbacks) -> [
	% init(Context) -> ParameterizedModule
	% Called at the start of the request.
	{init, 1},      

	% render(Context) -> NewContext
	% Called at the end of the request, before sending
	% a response back to the browser.
	{render, 1},
	
	% get_set(Key, Function, TTL) -> {ok, Value, ParameterizedModule}
	% Return the cache value associated with Key. If it is not found,
	% then run the Function, store the resulting value in cache under
	% Key, and return the value.
	{get_set, 3}, 
	
	% clear(Key) -> ParameterizedModule
	% Remove a value from cache.
	{clear, 1},
	
	% clear_all() -> ParameterizedModule
	% Clear all values from cache.
	{clear_all, 0}
];

behaviour_info(_) -> undefined.
