% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_process_registry_handler).
-behaviour (process_registry_handler).
-include ("wf.inc").
-export ([
	start/0,
	init/2, 
	finish/2,
	get_pid/3,
	get_pid/4
]).

-define (TABLE, process_registry).

start() ->
	% TODO - this needs to be way more robust.
	F = fun() ->
		case lists:member(?TABLE, ets:all()) of
			true -> ok;
			false -> 
				ets:new(?TABLE, [set, public, named_table]), 
				timer:sleep(infinity) 
		end
	end,
	erlang:spawn(F).

init(_Config, State) -> 
	{ok, State}.

finish(_Config, State) ->
	{ok, State}.
	
get_pid(Key, _Config, State) -> 
	Pid = case ets:lookup(?TABLE, Key) of
		[] -> undefined;
		[{Key, Value}] -> Value
	end,
	{ok, Pid, State}.

get_pid(Key, Function, Config, State) ->
	{ok, Pid, State1} = get_pid(Key, Config, State),
	Pid1 = case Pid /= undefined andalso is_pid(Pid) andalso is_process_alive(Pid) of
		true  -> Pid;
		false -> 
			NewPid = erlang:spawn(Function),
			ets:insert(?TABLE, {Key, NewPid}),
			NewPid
	end,
	{ok, Pid1, State1}.