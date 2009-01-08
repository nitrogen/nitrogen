% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% Contributions from Dave Peticolas
% See MIT-LICENSE for licensing information.
	
-module (wf_session).
-include ("wf.inc").
-export ([
	ensure_session/0,
	user/0, user/1, clear_user/0,
	role/1, role/2, clear_roles/0,
	session/1, session/2, clear_session/0,
	start_link/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

%%% USER %%%
user() -> session(wf_user).
user(User) -> session(wf_user, User).
clear_user() -> user(undefined).

%%% ROLES %%%
role(Role) ->	   sets:is_element(Role, get_roles()).
role(Role, true) -> session(wf_roles, sets:add_element(Role, get_roles()));
role(Role, _) ->	session(wf_roles, sets:del_element(Role, get_roles())).

get_roles() -> 
	case session(wf_roles) of
		undefined -> sets:new();
		Roles -> Roles
	end.

clear_roles() -> 
	session(wf_roles, sets:new()).

%%% SESSION %%%

session(Key) ->
	{ok, Value} = gen_server:call(get_session_pid(), {get, Key}),
	Value.

session(Key, Value) -> 
	ok = gen_server:call(get_session_pid(), {put, Key, Value}),
	Value.

clear_session() -> 
	ok = gen_server:call(get_session_pid(), clear).


%%% ENSURE/CREATE SESSION %%%
	
ensure_session() -> 
	case wf_platform:get_cookie(wf) of
			undefined -> 
				sign_key();
			Value ->
				case wf:depickle(Value) of
					{Pid, Unique} -> 
						ensure_session_is_alive(Pid, Unique);
					_ -> 
						sign_key()
				end
		end.

ensure_session_is_alive(Pid, Unique) ->
	case wf_session_server:get_session(Unique) of
		{ok, Pid} ->
			case ping(Pid) of
				ok ->
					put(wf_session, Pid),
					drop_session_cookie(Pid, Unique);
				timeout ->
					sign_key()
			end;
		_ ->
			sign_key()
	end.

sign_key() -> 	
	Unique = erlang:make_ref(),
	{ok, Pid} = wf_session_server:sign_key(Unique),
	put(wf_session, Pid),
	drop_session_cookie(Pid, Unique).

drop_session_cookie(Pid, Unique) ->
	Session = wf:pickle({Pid, Unique}),
	Timeout = nitrogen:get_session_timeout(),
	wf_platform:set_cookie(wf, Session, "/", Timeout).

get_session_pid() -> get(wf_session).


%%% gen_server callbacks %%%

start_link() ->
	gen_server:start_link(?MODULE, [], []).

init([]) ->
	{ok, [], timeout()}.


handle_call(ping, _From, Session) ->
	{reply, ok, Session, timeout()};

handle_call({put, Key, undefined}, _From, Session) ->
	{reply, ok, lists:keydelete(Key, 1, Session), timeout()};

handle_call({put, Key, Value}, _From, Session) ->
	{reply, ok, lists:keystore(Key, 1, Session, {Key, Value}), timeout()};

handle_call({get, Key}, _From, Session) ->
	Value = case lists:keysearch(Key, 1, Session) of
				{value, {Key, V}} -> V;
				false -> undefined
			end,
	{reply, {ok, Value}, Session, timeout()};

handle_call(clear, _From, _Session) ->
	{reply, ok, [], timeout()}.


handle_cast(_Msg, State) ->
	{noreply, State, timeout()}.

handle_info(timeout, State) ->
	{stop, normal, State};

handle_info(_Info, State) ->
	{noreply, State, timeout()}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


timeout() ->
	nitrogen:get_session_timeout() * 60 * 1000.
	
ping(Pid) ->
	try
		ok = gen_server:call(Pid, ping)
	catch
		exit:{timeout, _} ->
		timeout
	end.
