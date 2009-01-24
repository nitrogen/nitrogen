% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (nitrogen).
-include ("wf.inc").
-export ([
	start/0,
	stop/0,
	init/1,
	start_server/0,
	route/1,
	request/1,
	get_platform/0,
	get_port/0,
	get_session_timeout/0,
	get_sign_key/0,
	get_wwwroot/0,
	get_hooks_module/0
]).

start() -> supervisor:start_link(?MODULE, []).

init(_Args) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 1000,
	MaxSecondsBetweenRestarts = 3600,
	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
	NitrogenServer = {quickstart_sup, {nitrogen, start_server, []}, permanent, 2000, worker, dynamic},
	SessionServer  = {wf_session_server, {wf_session_server, start_link, []}, permanent, 2000, worker, [wf_session_server]},
	SessionSup     = {wf_session_sup, {wf_session_sup, start_link, []}, permanent, 2000, supervisor, [wf_session_sup]},
	{ok,{SupFlags,[NitrogenServer, SessionServer, SessionSup]}}.

start_server() ->
	HooksModule = get_hooks_module(),
	code:ensure_loaded(HooksModule),
	
	Result = case get_platform() of
		yaws     -> nitrogen_yaws_app:start();
		mochiweb -> nitrogen_mochiweb_app:start();
		inets    -> nitrogen_inets_app:start()
	end,

	io:format("~n~n---~n"),
	io:format("Nitrogen is now running on ~s.~n", [get_platform()]),
	io:format("Serving files from: ~s.~n", [get_wwwroot()]),
	io:format("Open your browser to: http://localhost:~p~n", [get_port()]),
	io:format("---~n~n"),

	Result.

stop() -> 
	case get_platform() of
		yaws     -> nitrogen_yaws_app:stop();
		mochiweb -> nitrogen_mochiweb_app:stop();
		inets    -> nitrogen_inets_app:stop()
	end.	
	
%%% DEFAULT ROUTE AND REQUEST %%%

route(Path) -> wf_utils:path_to_module(Path).
request(_) -> ok.		

%%% GET CONFIG SETTINGS %%%
	
get_platform() -> 
	case application:get_env(platform) of
		{ok, Val} -> Val;
		_ -> inets
	end.
	
get_session_timeout() ->
	case application:get_env(session_timeout) of
		{ok, Val} -> Val;
		_ -> 20
	end.
	
get_port() -> 
	case application:get_env(port) of 
		{ok, Val} -> Val;
		_ -> 8000
	end.

get_wwwroot() -> 
	case application:get_env(wwwroot) of
		{ok, Val} -> Val;
		_ -> "./wwwroot"
	end.
	
get_sign_key() -> 
	case application:get_env(sign_key) of
		{ok, Val} -> Val;
		_ -> throw("You must declare a sign_key!")
	end.

get_hooks_module() ->
	case application:get_env(hooks_module) of
		{ok, Module} ->
			Module;
		undefined ->
			{ok, {Module, _}} = application:get_key(mod),
			Module
	end.
