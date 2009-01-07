% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module(wf_cache_server).
% -behaviour(gen_server).
% 
% -export([start_link/0]).
% -export([read/1, write/1, delete/1]).
% -export([init/1, handle_call/3, handle_cast/2]).
% 
% %%% CACHE_SERVER FUNCTIONS %%%
% 
% read(Key) -> gen_server:call(cache_server, {read, Key}).
% write(Tuple) -> gen_server:call(cache_server, {write, Tuple}).
% delete(Key) -> gen_server:call(cache_server, {delete, Key}).
% 
% %%% BEHAVIOR FUNCTIONS %%%
% 
% start_link() -> gen_server:start_link({local, cache_server}, ?MODULE, [], []).
% 
% init(_Args) ->
% 	Table = ets:new(cache_server, [public, set, {keypos,2}]),
%   {ok, Table}.
% 
% handle_call({read, Key}, _From, Table) -> 
% 	Result = ets:lookup(Table, Key),
% 	{reply, Result, Table}.
% 	
% handle_cast({write, Key, Value}, Table) ->
% 	ets:insert(Table, {Key, Value}),
% 	{noreply, Table};
% 	
% handle_cast({delete, Key}, Table) ->
% 	ets:delete(Table, Key),
% 	{noreply, Table}.
% 	
% code_change(_OldVsn, State, _Extra) -> {ok, State}.