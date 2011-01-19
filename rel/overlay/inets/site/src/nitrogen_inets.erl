-module(nitrogen_inets).
-export ([do/1]).
	
do(Info) ->
	RequestBridge = simple_bridge:make_request(inets_request_bridge, Info),
	ResponseBridge = simple_bridge:make_response(inets_response_bridge, Info),
	nitrogen:init_request(RequestBridge, ResponseBridge),
	nitrogen:run().
