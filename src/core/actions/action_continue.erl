% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_continue).
-include ("wf.inc").
-compile(export_all).

% This action is used internally by Nitrogen.
render_action(Record, Context) ->
	% Start up the continuation function...
	Tag = Record#continue.tag,
	Function = Record#continue.function,
	Interval = Record#continue.interval,
	Timeout = Record#continue.timeout,
	Pid = spawn(fun() -> start_continuation_wrapper(Tag, Function, Interval, Timeout) end),

	% Create the postback event...
	Delegate = Record#continue.delegate,
	ContinuationEvent = create_continuation_event(Delegate, Pid, Interval),
	{ok, ContinuationEvent, Context}.

	
register(Delegate, Pid, Interval, Context) ->
	ContinuationEvent = create_continuation_event(Delegate, Pid, Interval),
	{ok, Context1} = wff:wire(ContinuationEvent, Context),
	{ok, Context1}.
	
create_continuation_event(Delegate, Pid, Interval) ->
	#event { type=continuation, delegate=Delegate, delay=Interval, postback=Pid }.

get_result(Pid) ->
	case wf_utils:is_process_alive(Pid) of
		true -> 
			Pid!{get_result, self()},
			Result = receive M -> M end,
			Result;
		false ->
			undefined
	end.	

wait([]) -> ok;
wait([H|T]) -> 
	receive after 100 -> ok end,
	case wf_utils:is_process_alive(H) of
		true -> wait([H|T]);
		false -> wait(T)
	end;
wait(Pid) -> wait([Pid]).

%%% PRIVATE FUNCTIONS %%%
			
start_continuation_wrapper(Tag, Function, Interval, Timeout) ->
	WrapperPid = self(),
	Pid = spawn(fun() -> 
		Result = try Function() catch Type : Message -> {error, Type, Message} end,
		WrapperPid!{result, self(), Result} 
	end),
	erlang:send_after(Timeout, self(), timeout),
	continuation_wrapper(Tag, Pid, Interval, no_result).

continuation_wrapper(Tag, Pid, Interval, Result) ->
	receive 
		{result, Pid, NewResult} -> 
			continuation_wrapper(Tag, Pid, Interval, NewResult);
			
		{get_result, CallingPid} when Result == no_result ->
			CallingPid!{running, Interval},
			continuation_wrapper(Tag, Pid, Interval + 100, Result);
			
		{get_result, CallingPid} -> 
			CallingPid!{done, Tag, Result};
		
		timeout ->
			continuation_wrapper(Tag, Pid, Interval, timeout)
			
		after 5 * 60 * 1000 -> stop_running
	end.