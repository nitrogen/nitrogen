% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_session).
-include ("wf.inc").
-export ([
	ensure_session/0,
	user/0, user/1, clear_user/0,
	role/1, role/2, clear_roles/0,
	session/1, session/2, clear_session/0
]).

%%% USER %%%
user() -> session(wf_user).
user(User) -> session(wf_user, User).
clear_user() -> user(undefined).

%%% ROLES %%%
role(Role) ->       sets:is_element(Role, get_roles()).
role(Role, true) -> session(wf_roles, sets:add_element(Role, get_roles()));
role(Role, _) ->    session(wf_roles, sets:del_element(Role, get_roles())).
	
get_roles() -> 
	case session(wf_roles) of
		undefined -> sets:new();
		Roles -> Roles
	end.
	
clear_roles() -> 
	session(wf_roles, sets:new()).
	
%%% SESSION %%%

session(Key) -> 
	Pid = get_session_pid(),
	Pid!{self(), get, Key},
	receive 
		{Pid, get_complete, Key, Value} -> Value 
	end.

session(Key, Value) -> 
	Pid = get_session_pid(),
	Pid!{self(), put, Key, Value},
	receive
		{Pid, put_complete} -> Value
	end.
	
clear_session() -> 
	Pid = get_session_pid(),
	Pid!{self(), clear},
	receive
		{Pid, clear_complete} -> ok
	end.

%%% ENSURE/CREATE SESSION %%%
	
ensure_session() -> 
	case wf_platform:get_cookie(wf) of
			undefined -> 
				create_session();
			Value ->
				case wf:depickle(Value) of
					{Pid, Unique} -> 
						ensure_session_is_alive(Pid, Unique);
					_ -> 
						create_session()
				end
		end.
	
ensure_session_is_alive(Pid, Unique) ->
	case wf_utils:is_process_alive(Pid) of
		true -> ensure_session_matches_unique(Pid, Unique);
		false -> create_session()
	end.

ensure_session_matches_unique(Pid, Unique) ->
	Pid!{self(), verify, Unique},
	receive 
		{Pid, verify_ok} -> 
			put(wf_session, Pid),
			drop_session_cookie(Pid, Unique);
		_Other -> 
			create_session()
	end.	
		
create_session() -> 	
	Unique = erlang:make_ref(),
	Pid = erlang:spawn(fun() -> session_loop(Unique, []) end),
	put(wf_session, Pid),
	drop_session_cookie(Pid, Unique).
	
drop_session_cookie(Pid, Unique) ->
	Session = wf:pickle({Pid, Unique}),
	Timeout = wf_global:session_timeout(),
	wf_platform:set_cookie(wf, Session, "/", Timeout).

get_session_pid() -> get(wf_session).

session_loop(Unique, Session) ->
	Timeout = wf_global:session_timeout(),
	receive
		{Pid, verify, Unique} -> 
			Pid!{self(), verify_ok},
			session_loop(Unique, Session);
			
		{Pid, verify, _} ->      
			Pid!{self(), verify_failed},
			session_loop(Unique, Session);
		
		{Pid, put, Key, undefined} ->
			Session1 = lists:keydelete(Key, 1, Session),
			Pid!{self(), put_complete},
			session_loop(Unique, Session1);
		
		{Pid, put, Key, Value} -> 
			Session1 = lists:keystore(Key, 1, Session, {Key, Value}),
			Pid!{self(), put_complete},
			session_loop(Unique, Session1);

		{Pid, get, Key} -> 
			Value = case lists:keysearch(Key, 1, Session) of
				{value, {Key, V}} -> V;
				false -> undefined
			end,
			Pid!{self(), get_complete, Key, Value},
			session_loop(Unique, Session);

		{Pid, clear} -> 
			Pid!{self(), clear_complete},
			session_loop(Unique, [])

	after Timeout * 60 * 1000 -> stop
	end.
	
