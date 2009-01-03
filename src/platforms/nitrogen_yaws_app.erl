% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (nitrogen_yaws_app).
-export ([start/0, stop/0]).

start() ->
	% Initialize Nitrogen.
	wf:init(),
	
	% Start the Yaws server.
	SC = [
		{listen, {0,0,0,0}},
	  {port, nitrogen:get_port()},
	  {appmods, [{"/web", wf_yaws}]
	}],
	GC = [
		{logdir, "./logs"},
		{cache_refresh_secs, 0}
	],
	yaws:start_embedded(nitrogen:get_wwwroot(), SC, GC),
	Pid = application_controller:get_master(yaws),
	link(Pid),
	{ok, Pid}.
	
stop() -> 
	% Stop the Yaws server.
	Pid = application_controller:get_master(yaws),
	exit(Pid, kill),
	ok.
	
	