% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (nitrogen).
-export ([init_request/2, run/0]).

init_request(RequestBridge, ResponseBridge) ->
	wf_context:init_context(RequestBridge, ResponseBridge).

run() -> wf_core:run().