% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (nitrogen).
-export ([init_request/2, handler/2, run/0]).

init_request(RequestBridge, ResponseBridge) ->
    wf_context:init_context(RequestBridge, ResponseBridge).

handler(Module, Config) ->
    wf_handler:set_handler(Module, Config).

run() -> 
    wf_core:run().
