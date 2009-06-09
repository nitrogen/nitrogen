% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_process_cabinet_handler).
-behaviour (process_cabinet_handler).
-include ("simplebridge.hrl").
-include ("wf.inc").
-export ([
	init/1, 
	finish/2,
	get/3,
	get_set/4
]).

-define (TABLE, process_cabinet).

init(Context) -> 
	case lists:member(?TABLE, ets:all()) of
		true -> ok;
		false -> ets:new(?TABLE, [set, public, named_table])
	end,
	{ok, Context, []}.

finish(Context, State) ->
	{ok, Context, State}.
	
get(Key, Context, State) -> 
	Pid = case ets:lookup(?TABLE, Key) of
		[] -> undefined;
		[{Key, Value}] -> Value
	end,
	{ok, Pid, Context, State}.

get_set(Key, Function, Context, State) ->
	{ok, Pid, Context1, State1} = get(Key, Context, State),
	Pid1 = case Pid /= undefined andalso is_pid(Pid) andalso is_process_alive(Pid) of
		true  -> Pid;
		false -> erlang:spawn(Function)
	end,
	{ok, Pid1, Context1, State1}.