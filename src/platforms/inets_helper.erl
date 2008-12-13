% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (inets_helper).
-export ([start/0]).

start() ->
	% Initialize Nitrogen.
	wf:init(),

	inets:start(),
	inets:start(httpd, [
		{port, 8000},
		{document_root, "./content/wwwroot"},
		{server_root, "."},
		{bind_address, "localhost"},
		{server_name, "localhost"},
		{modules, [wf_inets, mod_head, mod_get]},
		{mime_types, [{"css", "text/css"}, {"js", "text/javascript"}, {"html", "text/html"}]}
	]),
	io:format("~n~n---~n"),
	io:format("Nitrogen is now running, using inets:httpd().~n"),
	io:format("Open your browser to: http://localhost:8000~n"),
	io:format("---~n~n").
