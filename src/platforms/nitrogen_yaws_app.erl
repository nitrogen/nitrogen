% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (nitrogen_yaws_app).
-include ("wf.inc").
-include("yaws.hrl").
-include("yaws_api.hrl").
-export ([start/0, stop/0]).

start() ->
	% Initialize Nitrogen.
	wf:init(),

	% Set up Yaws Configuration...
	{ok, App} = application:get_application(),
	Id = atom_to_list(App),
	SC = #sconf {
		docroot = nitrogen:get_wwwroot(),
		port=nitrogen:get_port(),
		appmods = [{"/web", wf_yaws}],
		listen = {0, 0, 0, 0}		
	},
	DefaultGC = yaws_config:make_default_gconf(false, Id),
	GC = DefaultGC#gconf {
		logdir = "./logs",
		cache_refresh_secs = 5
	},

	% Following code adopted from yaws:start_embedded/4. 
	% This will need to change if Yaws changes!!!
	ok = application:set_env(yaws, embedded, true),
	ok = application:set_env(App, embedded, true),
	ok = application:set_env(App, id, Id),
	{ok, Pid} = yaws_sup:start_link(),
	yaws_config:add_yaws_soap_srv(GC),
	SCs = yaws_config:add_yaws_auth([SC]),
	yaws_api:setconf(GC, [SCs]),
	{ok, Pid}.
	
stop() -> 
	% Stop the Yaws server.
	Pid = application_controller:get_master(yaws),
	exit(Pid, kill),
	ok.
	
	