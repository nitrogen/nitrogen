-module (test_inets).
-include ("simplebridge.hrl").
-include ("../include/httpd.hrl").
-export ([
	start/0,
	do/1
]).

start() ->
	ets:new(process_cabinet, [set, public, named_table]),
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
	case RequestBridge:path() of
		"/web" ++ _ -> do_nitrogen(RequestBridge, ResponseBridge);
		_ -> {proceed, Info#mod.data}
	end.

do_nitrogen(RequestBridge, ResponseBridge) ->
	Context = wf_context:make_context(RequestBridge, ResponseBridge),
	wf_core:run(Context).