-module (quickstart_mochiweb_app).
-include ("wf.inc").

-export ([
	start/2
]).

start(_, _) ->
	default_process_cabinet_handler:start(),
	Options = [{ip, "127.0.0.1"}, {port, 8000}],
	Loop = fun loop/1,
	mochiweb_http:start([{name, mochiweb_example_app}, {loop, Loop} | Options]).

loop(Req) ->
	RequestBridge = simple_bridge:make_request(mochiweb_request_bridge, {Req, "./wwwroot"}),
	ResponseBridge = simple_bridge:make_response(mochiweb_response_bridge, {Req, "./wwwroot"}),
	nitrogen:init_request(RequestBridge, ResponseBridge),
	nitrogen:run().