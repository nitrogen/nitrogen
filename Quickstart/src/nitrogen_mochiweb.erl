-module(nitrogen_mochiweb).
-export ([start/2]).
-include_lib("nitrogen/include/wf.hrl").

start(_, _) ->
    % Read config...
    {ok, BindAddress} = application:get_env(mochiweb, bind_address),
    {ok, Port} = application:get_env(mochiweb, port),
    {ok, ServerName} = application:get_env(mochiweb, server_name),
    {ok, DocRoot} = application:get_env(mochiweb, document_root),

    io:format("Starting Mochiweb Server (~s) on ~s:~p, root: '~s'~n", [ServerName, BindAddress, Port, DocRoot]),

    % Start Mochiweb...
    Options = [
        {name, ServerName},
        {ip, BindAddress}, 
        {port, Port},
        {loop, fun loop/1}
    ],
    mochiweb_http:start(Options).

loop(Req) ->
    {ok, DocRoot} = application:get_env(mochiweb, document_root),
    RequestBridge = simple_bridge:make_request(mochiweb_request_bridge, {Req, DocRoot}),
    ResponseBridge = simple_bridge:make_response(mochiweb_response_bridge, {Req, DocRoot}),
    nitrogen:init_request(RequestBridge, ResponseBridge),
    nitrogen:run().
