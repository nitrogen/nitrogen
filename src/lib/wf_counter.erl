% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_counter).
-include ("wf.inc").
-export ([start/1, count/1, count/2, format/2, display/2]).

start(ProcessName) ->
	case whereis(ProcessName) of
		undefined ->
			Pid = spawn(fun() -> counter_loop(0, 0) end),
			register(ProcessName, Pid);
		_ -> ok
	end.
	
count(ProcessName) -> count(ProcessName, 1).
	
count(ProcessName, Incr) ->
	ProcessName!{count, Incr}.
	
format(ProcessName, FormatString) ->
	ProcessName!{get, self()},
	receive
		{ok, TotalCount, CountSinceLastCall} -> io_lib:format(FormatString, [TotalCount, CountSinceLastCall])
	end.	

display(ProcessName, FormatString) ->
	ProcessName!{get, self()},
	receive
		{ok, TotalCount, CountSinceLastCall} -> io:format(FormatString, [TotalCount, CountSinceLastCall])
	end.
	
counter_loop(TotalCount, CountSinceLastCall) ->
	receive
		{count, Incr} ->
			counter_loop(TotalCount + Incr, CountSinceLastCall + Incr);
			
		{get, Pid} ->
			Pid!{ok, TotalCount, CountSinceLastCall},
			counter_loop(TotalCount, 0);
			
		Other -> 
			?LOG("(~p:~p) Unexpected Message: ~p~n", [?MODULE, ?LINE, Other])
	end.
			