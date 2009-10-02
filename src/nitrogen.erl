% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (nitrogen).
-include ("wf.inc").
-export ([
	start/0,
	start/1,
	stop/0,
	init/1,
	start_server/0,
	route/1,
	request/1,
	get_platform/0,
	get_host/0,
	get_ip/0,
	get_port/0,
	get_session_timeout/0,
	get_sign_key/0,
	get_wwwroot/0,
	get_templateroot/0,
	get_hooks_module/0,
	get_scratch_directory/0
]).

start() -> start(undefined).

start(ServingApp) when is_atom(ServingApp) -> 
	supervisor:start_link(?MODULE, [ServingApp]).

init([ServingApp]) ->
	application:set_env(nitrogen, serving_app, ServingApp),
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

	Host = case get_ip() of
		{0,0,0,0} -> "localhost";
		{127,0,0,1} -> "localhost";
		{W,X,Y,Z} -> wf:f("~B.~B.~B.~B", [W, X, Y, Z])
	end,

	io:format("~n~n---~n"),
	io:format("Nitrogen is now running on ~s.~n", [get_platform()]),
	io:format("Serving files from: ~s~n", [get_wwwroot()]),
	io:format("Template root is at: ~s~n", [get_templateroot()]),
	io:format("Open your browser to: http://~s:~p~n", [Host, get_port()]),
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
	case get_env(serving_app(), platform) of
		{ok, Val} -> Val;
		_ -> inets
	end.
	
get_session_timeout() ->
	case get_env(serving_app(), session_timeout) of
		{ok, Val} -> Val;
		_ -> 20
	end.
	
get_host() -> 
	case get_env(serving_app(), host) of 
		{ok, Val} -> Val;
		_ -> "localhost"
	end.

get_ip() ->
	case get_env(serving_app(), ip) of
		{ok, Val} when is_tuple(Val) -> 
			Val;
		{ok, Val} ->
			case inet_parse:address(Val) of
				{ok, Ip} -> Ip;
				_ -> throw("Invalid ip configuration string")	
			end;
		_ -> 
			{0,0,0,0}
	end.

get_port() -> 
	case get_env(serving_app(), port) of 
		{ok, Val} -> Val;
		_ -> 8000
	end.

get_wwwroot() -> 
	case get_env(serving_app(), wwwroot) of
		{ok, Val} -> Val;
		_ -> "./wwwroot"
	end.

get_templateroot() ->
    case application:get_env(templateroot) of
        {ok, Val} -> Val;
        _ -> "./"
    end.	
	
get_sign_key() -> 
	case get_env(serving_app(), sign_key) of
		{ok, randomized} -> 
		    Key = wf_utils:guid(),     
		    set_env(serving_app(), sign_key, Key),
		    Key;

		{ok, Val} -> 
		    Val;

		_ -> throw("You must declare a sign_key!")
	end.

get_hooks_module() ->
	case get_env(serving_app(), hooks_module) of
		{ok, Module} ->
			Module;
		_ ->
			{ok, {Module, _}} = application:get_key(mod),
			Module
	end.
	
get_scratch_directory() ->
	case get_env(serving_app(), scratch_directory) of
		{ok, Val} -> Val;
		_ -> "./scratch"
	end.

serving_app() -> 
	case get_env(nitrogen, serving_app) of 
		{ok, Val} -> Val;
		_ -> undefined
	end.
	
get_env(undefined, Key) -> 
	Value = application:get_env(Key),
	Value;
	
get_env(App, Key) -> 
	Value = application:get_env(App, Key),
	Value.

set_env(undefined, Key, Val) ->
	{ok, App} = application:get_application(),
	ok = application:set_env(App, Key, Val);

set_env(App, Key, Val) ->
	ok = application:set_env(App, Key, Val).
