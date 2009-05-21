-module (test_inets).
-include ("simplebridge.hrl").
-include ("wf.inc").
-export ([
	start/0,
	do/1
]).

start() ->
	inets:stop(),
	inets:start(),
	inets:start(httpd, [
		{port, 8000},
		{document_root, "./wwwroot"},
		{server_root, "."},
		{bind_address, {0,0,0,0}},
		{server_name, "localhost"},
		{modules, [?MODULE, mod_head, mod_get]},
		{mime_types, [{"css", "text/css"}, {"js", "text/javascript"}, {"html", "text/html"}]}
	], stand_alone).
	
do(Info) ->
	RequestBridge = request_bridge:make(inets_request_bridge, Info),
	ResponseBridge = response_bridge:make(inets_response_bridge),
	Context = wf_context:make_context(RequestBridge, ResponseBridge),
	?PRINT(Context),
	wf_core:run(Context).