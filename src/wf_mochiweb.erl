-module (wf_mochiweb).
-export ([loop/1, loop/2]).

loop(Req) ->
	Path = Req:get(path),
	Module = wf:path_to_module(Path),
	loop(Req, Module).

loop(Req, Module) ->
	wf_platform:init_mochiweb_request(Req),
	wf_handle:handle_request(Module).