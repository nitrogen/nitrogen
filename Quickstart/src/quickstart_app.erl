-module (quickstart_app).
-export ([start/2, stop/0, do/1]).
-include_lib ("nitrogen/include/wf.hrl").
-define(PORT, 8000).


start(_, _) ->
    inets:start(),
    {ok, Pid} = inets:start(httpd, [
        {port, ?PORT},
        {server_name, "nitrogen"},
        {server_root, "."},
        {document_root, "./static"},
        {modules, [?MODULE]},
        {mime_types, [{"css", "text/css"}, {"js", "text/javascript"}, {"html", "text/html"}]}
    ]),
    link(Pid),
    {ok, Pid}.

stop() ->
    httpd:stop_service({any, ?PORT}),
    ok.

do(Info) ->
    RequestBridge = simple_bridge:make_request(inets_request_bridge, Info),
    ResponseBridge = simple_bridge:make_response(inets_response_bridge, Info),
    nitrogen:init_request(RequestBridge, ResponseBridge),

    %% Uncomment for basic authentication...
    %% nitrogen:handler(http_basic_auth_security_handler, basic_auth_callback_mod),

    nitrogen:run().
