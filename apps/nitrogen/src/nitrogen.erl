% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (nitrogen).
-export ([init_request/2, handler/2, run/0, start_link/1, start_link/2,
         do/1, out/1, do_mochiweb/2]).

init_request(RequestBridge, ResponseBridge) ->
    wf_context:init_context(RequestBridge, ResponseBridge).

handler(Module, Config) ->
    wf_handler:set_handler(Module, Config).

run() -> 
    wf_core:run().


handling_module() ->
    {ok, Root} = application:get_env(nitrogen_handler_module),
    Root.

start_link(Mod) ->
    {ok, App} = application:get_application(),
    application:set_env(App, nitrogen_handler_module, Mod),
    start_link(Mod:http_server(), Mod).  % get config data from the .app file
 
start_link(inets, Mod) ->
    {ok, Pid} = 
        inets:start(httpd, 
                    [{port,           Mod:serverport()} 
                     ,{server_name,   Mod:servername()} 
                     ,{bind_address,  Mod:serverip()} 
                     ,{server_root,   "."} 
                     ,{document_root, Mod:docroot()} 
                     ,{modules,       [?MODULE]} 
                     ,{mime_types,    [{"css",  "text/css"}, 
                                       {"js",   "text/javascript"}, 
                                       {"html", "text/html"}]} 
                    ]),
    link(Pid),
    {ok, Pid};

start_link(mochiweb, Mod) -> 
    Options = [{port,   Mod:serverport()} 
               ,{name,  Mod:servername()} 
               ,{ip,    Mod:serverip()} 
               ,{loop,  fun(Req) -> do_mochiweb(Req, Mod) end} 
              ], 
    {ok, Pid} = mochiweb_http:start(Options), 

    link(Pid), 
    {ok, Pid}. 

%start_link(yaws, Mod) -> 
%        SC = #sconf { 
%                appmods     = [{"/", ?MODULE}], 
%                docroot     = Mod:docroot(), 
%                port        = Mod:serverport(), 
%                servername  = Mod:servername(), 
%                listen      = Mod:serverip() 
%        }, 
%        DefaultGC = yaws_config:make_default_gconf(false, redhot2), 
%        GC = DefaultGC#gconf { 
%                logdir = redhot2:log_dir(), 
%                cache_refresh_secs = 5 
%        },
% 
%        % Following code adopted from yaws:start_embedded/4. 
%        % This will need to change if Yaws changes!!! 
%        ok = application:set_env(yaws, embedded, true), 
%        {ok, Pid} = yaws_sup:start_link(), 
%        yaws_config:add_yaws_soap_srv(GC), 
%        SCs = yaws_config:add_yaws_auth([SC]), 
%        yaws_api:setconf(GC, [SCs]), 
%        {ok, Pid}. 

% Inets handler
do(Info) -> 
    RequestBridge = simple_bridge:make_request(inets_request_bridge, 
Info), 
    ResponseBridge = 
simple_bridge:make_response(inets_response_bridge, Info), 
    nitrogen:init_request(RequestBridge, ResponseBridge),
    Mod = handling_module(),
    Mod:handlers(),
    nitrogen:run().

% Yaws handler 
out(Info) -> 
    RequestBridge = simple_bridge:make_request(yaws_request_bridge, 
Info), 
    ResponseBridge = simple_bridge:make_response(yaws_response_bridge, 
Info), 
    nitrogen:init_request(RequestBridge, ResponseBridge), 
    Mod = handling_module(),
    Mod:handlers(),
    nitrogen:run().

% Mochiweb handler 
do_mochiweb(Info, Mod) -> 
    RequestBridge = 
        simple_bridge:make_request(mochiweb_request_bridge,
                                   {Info, Mod:docroot()}), 
    ResponseBridge = 
        simple_bridge:make_response(mochiweb_response_bridge,
                                    {Info, Mod:docroot()}),
    nitrogen:init_request(RequestBridge, ResponseBridge),
    Mod:handlers(),
    nitrogen:run().
