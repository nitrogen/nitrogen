% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_cache).
% -include ("wf.inc").
% -export ([
% 	init/0, 
% 	cache/2, 
% 	cache/3
% ]).
% 
% -record(cacheitem, {key, value, function, options, pid}).
% 
% % Cache Options:
% %	{ttl, Seconds} -> time to live until cache is expired.
% % {slide, Boolean} -> slide expiration if the cached object is read.
% 
% % Memory only cache for simplicity.
% % Store in mnesia or ets.
% 
% %%% READ AND WRITE CACHE %%%	
% 
% cache(Key, Function) ->
% 	DefaultOptions = [
% 		{ttl, infinity}, 
% 		{slide, true}
% 	], 
% 	cache(Key, Function, DefaultOptions).
% 
% cache(Key, Function, Options) -> 
% 	case wf_cache_server:read(Key) of
% 		[CacheItem] -> return_cache_item(CacheItem);
% 		_ -> recache(Key, Function, Options)
% 	end.
% 
% %% return_cache_item/1 - We found a cache item.
% %% Check the options to see if this is a sliding cache.
% %% If so, then signal a cache hit.
% return_cache_item(CacheItem) ->
% 	% Sliding Cache? Signal cache hit...
% 	Options = CacheItem#cacheitem.options,
% 	case proplists:get_value(slide, Options) of
% 		false -> ignore;
% 		_ -> 
% 			Pid = CacheItem#cacheitem.pid,
% 			Pid!cache_hit
% 	end,
% 	
% 	% Return the item.
% 	CacheItem#cacheitem.value.	
% 	
% 	
% 	
% %%% WRITE TO THE CACHE TABLE %%%
% 
% recache(Key, Function, Options) ->
% 	% Read the value. 
% 	Value = Function(),
% 		
% 	% Get the time to live in milliseconds...
% 	TTL = case proplists:get_value(ttl, Options) of
% 		infinity -> infinity;
% 		undefined -> infinity;
% 		Seconds -> Seconds * 1000
% 	end,
% 	
% 	% Start the cache monitor function...
% 	Pid = erlang:spawn(fun() -> cache_loop(Key, TTL) end),
% 	
% 	% Write to the cache table...
% 	CacheItem = #cacheitem { key=Key, value=Value, function=Function, options=Options, pid=Pid },
% 	wf_cache_server:write(CacheItem),
% 	
% 	% Finally, return the value.
% 	Value.
% 	
% 
% 
% %%% CACHE LOOP %%%
% 
% %% cache_loop/2 - If there is a cache-hit, then restart the timer. 
% %% Otherwise, kill the cache after the time to live (TTL) has expired.
% cache_loop(Key, TTL) ->
% 	receive 
% 		cache_hit -> cache_loop(Key, TTL)
% 	after TTL ->
% 		wf_cache_server:delete(Key)
% 	end.
% 	
