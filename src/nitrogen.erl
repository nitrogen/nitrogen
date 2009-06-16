% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (nitrogen).
-export ([make_context/2, run/1]).

make_context(RequestBridge, ResponseBridge) ->
	wf_context:make_context(RequestBridge, ResponseBridge).

run(Context) -> 
	wf_core:run(Context).