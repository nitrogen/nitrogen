-module(nitrogen_mochiweb).
-export ([loop/1]).
-include_lib("nitrogen_core/include/wf.hrl").

loop(Req) ->
    {ok, DocRoot} = application:get_env(mochiweb, document_root),
    RequestBridge = simple_bridge:make_request(mochiweb_request_bridge, Req),
    ResponseBridge = simple_bridge:make_response(mochiweb_response_bridge, {Req, DocRoot}),
    nitrogen:init_request(RequestBridge, ResponseBridge),
    nitrogen:run().
