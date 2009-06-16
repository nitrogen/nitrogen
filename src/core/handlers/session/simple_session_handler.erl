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
	init/2, 
	finish/2,
	get_value/4, 
	set_value/4, 
	clear_value/3, 
	clear_all/2
]).

-define(TIMEOUT, 20000).

init(Context, _State) -> 
	% Check if a session exists for the user. If not, start one.
	Unique = case wff:depickle(wff:cookie("wf", Context)) of
		undefined -> erlang:md5(term_to_binary({now(), erlang:make_ref()}));
		Other -> Other
	end,
	{ok, Context1} = wff:cookie("wf", wff:pickle(Unique), Context),
	{ok, Context1, {session, Unique}}.

finish(Context, _State) -> 
	{ok, Context, []}.
	
get_value(Key, DefaultValue, Context, SessionName) -> 
	{ok, Pid, _Context1} = get_session_pid(SessionName, Context),
	Pid!{get_value, Key, self()},
	Value = receive 
		{ok, undefined} -> DefaultValue;
		{ok, Other} -> Other
	end,
	Value.
	
set_value(Key, Value, Context, SessionName) -> 
	{ok, Pid, Context1} = get_session_pid(SessionName, Context),
	Pid!{set_value, Key, Value, self()},
	receive ok -> ok end,	
	{ok, Context1, SessionName}.
	
clear_value(Key, Context, SessionName) ->
	{ok, Pid, Context1} = get_session_pid(SessionName, Context),
	Pid!{clear_value, Key, self()},
	receive ok -> ok end,	
	{ok, Context1, SessionName}.

clear_all(Context, SessionName) -> 
	{ok, Pid, Context1} = get_session_pid(SessionName, Context),
	Pid!{clear_all, self()},
	receive ok -> ok end,	
	{ok, Context1, SessionName}.
	
get_session_pid(SessionName, Context) ->
	{ok, _Pid, _Context1} = process_cabinet_handler:get_pid(SessionName, fun() -> session_loop([]) end, Context).

session_loop(Session) ->
	receive
		{get_value, Key, Pid} ->
			Value = case lists:keysearch(Key, 1, Session) of
				{value, {Key, V}} -> V;
				false -> undefined
			end,
			Pid!{ok, Value},
			session_loop(Session);
			
		{set_value, Key, Value, Pid} ->
			Session1 = lists:keystore(Key, 1, Session, {Key, Value}),
			Pid!ok,
			session_loop(Session1);			
			
		{clear_value, Key, Pid} ->	
			Session1 = lists:keydelete(Key, 1, Session),
			Pid!ok,
			session_loop(Session1);
			
		{clear_all, Pid} ->
			Pid!ok,
			session_loop([])	
				
	after ?TIMEOUT -> 
			exit(timed_out)
	end.
