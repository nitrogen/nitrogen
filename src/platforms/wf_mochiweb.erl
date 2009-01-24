% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_mochiweb).
-export ([loop/1, loop/2]).

loop(Req) ->
	wf_platform:init(wf_platform_mochiweb, Req),
	Path = Req:get(path),
	{Module, PathInfo} = wf_platform:route(Path),
	loop(Req, Module, PathInfo).
	
loop(Req, Module) -> loop(Req, Module, "").

loop(Req, Module, PathInfo) ->
	wf_platform:init(wf_platform_mochiweb, Req),
	wf_handle:handle_request(Module, PathInfo).