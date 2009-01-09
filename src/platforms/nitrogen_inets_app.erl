% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (nitrogen_inets_app).
-export ([start/0, stop/0]).

start() ->
	% Initialize Nitrogen.
	wf:init(),
	
	% Start the Inets server.	
	inets:start(),
	{ok, Pid} = inets:start(httpd, [
		{port, nitrogen:get_port()},
		{document_root, nitrogen:get_wwwroot()},
		{server_root, "."},
		{bind_address, any},
		{server_name, "localhost"},
		{modules, [wf_inets, mod_head, mod_get]},
		{mime_types, [{"css", "text/css"}, {"js", "text/javascript"}, {"html", "text/html"}]}
	], stand_alone),
	link(Pid),
	{ok, Pid}.


stop() -> 
	httpd:stop_service({any, nitrogen:get_port()}),
	ok.