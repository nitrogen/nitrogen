-module(nitrogen_yaws).
-export ([out/1]).

out(Arg) ->
  RequestBridge = simple_bridge:make_request(yaws_request_bridge, Arg),
  ResponseBridge = simple_bridge:make_response(yaws_response_bridge, Arg),
  nitrogen:init_request(RequestBridge, ResponseBridge),
  nitrogen:run().
