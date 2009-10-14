% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

% This is a "simple as possible" session handler. Unfortunately,
% due to time constraints, had to leave out some great code
% contributed by Dave Peticolas that fit Nitrogen sessions
% into a gen_server. My code below is far inferior. 
% Someone please make it better! - Rusty

-module (simple_session_handler).
-include ("wf.inc").
-behaviour (session_handler).
-export ([
	init/1, 
	finish/1,
	get_value/3, 
	set_value/3, 
	clear_all/1
]).

init(_) -> 
	% Get the session cookie...
	Unique = case wf:depickle(wf:cookie("wf")) of
		undefined -> erlang:md5(term_to_binary({now(), erlang:make_ref()}));
		Other -> Other
	end,
	{ok, {session, Unique}}.

finish({session, Unique}) -> 
	% Drop the session cookie...
	Timeout = wf:config_default(nitrogen_session_timeout, 20),
	ok = wf:cookie("wf", wf:pickle(Unique), "/", Timeout),
	{ok, []}.
	
get_value(Key, DefaultValue, SessionName) -> 
	{ok, Pid} = get_session_pid(SessionName),
	Ref = make_ref(),
	Pid!{get_value, Key, self(), Ref},
	Value = receive 
		{ok, undefined, Ref} -> DefaultValue;
		{ok, Other, Ref} -> Other
	end,
	Value.
	
set_value(Key, Value, SessionName) -> 
	{ok, Pid} = get_session_pid(SessionName),
	Ref = make_ref(),
	Pid!{set_value, Key, Value, self(), Ref},
	receive {ok, OldValue, Ref} -> ok end,	
	{ok, OldValue, SessionName}.
	
clear_all(SessionName) -> 
	{ok, Pid} = get_session_pid(SessionName),
	Ref = make_ref(),
	Pid!{clear_all, self(), Ref},
	receive {ok, Ref} -> ok end,	
	{ok, SessionName}.
	
get_session_pid(SessionName) ->
	Timeout = wf:config_default(nitrogen_session_timeout, 20),
	{ok, Pid} = process_cabinet_handler:get_pid(SessionName, fun() -> session_loop([], Timeout) end),
	{ok, Pid}.

session_loop(Session, Timeout) ->
	receive
		{get_value, Key, Pid, Ref} ->
			Value = case lists:keysearch(Key, 1, Session) of
				{value, {Key, V}} -> V;
				false -> undefined
			end,
			Pid!{ok, Value, Ref},
			session_loop(Session, Timeout);
			
		{set_value, Key, Value, Pid, Ref} ->
			OldValue = case lists:keysearch(Key, 1, Session) of
				{value, {Key, V}} -> V;
				false -> undefined
			end,
			Session1 = lists:keystore(Key, 1, Session, {Key, Value}),
			Pid!{ok, OldValue, Ref},
			session_loop(Session1, Timeout);			
			
		{clear_all, Pid, Ref} ->
			Pid!{ok, Ref},
			session_loop([], Timeout)	
				
	after Timeout * 60 * 1000 -> 
			exit(timed_out)
	end.
