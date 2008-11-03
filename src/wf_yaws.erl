-module (wf_yaws).
-include ("yaws_api.hrl").
-export ([out/1, out/2]).

out(Arg) ->
	Req = Arg#arg.req,
	{_, Path, _} = Req#http_request.path,
	Module = wf:path_to_module(Path),
	out(Arg, Module).

out(Arg, Module) -> 
	wf_platform:init_yaws_request(Arg),
	wf_handle:handle_request(Module).
