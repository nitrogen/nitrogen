-module (quickstart_mochiweb_app).
-include ("simplebridge.hrl").
-include ("wf.inc").

-export ([
	start/2
]).

start(_, _) ->
	{ok, IP} = application:get_env(ip),
	{ok, Port} = application:get_env(port),	
	Options = [{ip, IP}, {port, Port}],
	Loop = fun loop/1,
	mochiweb_http:start([{name, mochiweb_example_app}, {loop, Loop} | Options]).

loop(Req) ->
	Path = Req:get(path),
	{ok, DocRoot} = application:get_env(wwwroot),
	case Path of
		% Serve anything under /nitrogen statically.
		"/nitrogen" ++ _ -> Req:serve_file(tl(Path), DocRoot);
		"/css" ++ _ -> Req:serve_file(tl(Path), DocRoot);
		"/images" ++ _ -> Req:serve_file(tl(Path), DocRoot);
			
			
		_ -> 
			% Serve everything else under Nitrogen.
			do_nitrogen(Req)
	end.
	
do_nitrogen(Req) ->
	% Make request and response bridges...
	RequestBridge = simple_bridge:make_request(mochiweb_request_bridge, Req),
	ResponseBridge = simple_bridge:make_response(mochiweb_response_bridge, Req),
	
	% Make the Nitrogen context...
	Context = nitrogen:make_context(RequestBridge, ResponseBridge),
	nitrogen:run(Context).