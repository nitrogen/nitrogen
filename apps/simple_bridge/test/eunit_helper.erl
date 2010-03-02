-module (eunit_helper).
-export ([start/0]).

start() ->
    eunit:test(inets_request_bridge),
    eunit:test(inets_response_bridge).
