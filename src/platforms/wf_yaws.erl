% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_yaws).
-include ("wf.inc").
-include ("yaws_api.hrl").
-export ([out/1, out/2]).

out(Arg) ->
	Path = Arg#arg.server_path,
	Module = wf:path_to_module(Path),
	out(Arg, Module).

out(Arg, Module) -> 
	wf_platform:init(yaws, Arg),
	wf_handle:handle_request(Module).