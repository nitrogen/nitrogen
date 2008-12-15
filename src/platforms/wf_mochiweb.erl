% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_mochiweb).
-export ([loop/1, loop/2]).

loop(Req) ->
	Path = Req:get(path),
	{Module, PathInfo} = wf:path_to_module(Path),
	loop(Req, Module, PathInfo).
	
loop(Req, Module) -> loop(Req, Module, "").

loop(Req, Module, PathInfo) ->
	wf_platform:init(wf_platform_mochiweb, Req),
	wf_handle:handle_request(Module, PathInfo).