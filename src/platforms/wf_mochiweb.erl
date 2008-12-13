% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_mochiweb).
-export ([loop/1, loop/2]).

loop(Req) ->
	Path = Req:get(path),
	Module = wf:path_to_module(Path),
	loop(Req, Module).

loop(Req, Module) ->
	wf_platform:init(mochiweb, Req),
	wf_handle:handle_request(Module).