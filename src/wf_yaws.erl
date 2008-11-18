% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_yaws).
-include ("wf.inc").
-include ("yaws_api.hrl").
-export ([out/1, out/2, start/1]).

out(Arg) ->
	Path = Arg#arg.server_path,
	Module = wf:path_to_module(Path),
	out(Arg, Module).

out(Arg, Module) -> 
	wf_platform:init(yaws, Arg),
	wf_handle:handle_request(Module).

start(DocRoot) ->
	SC = [
		{listen, {0,0,0,0}},
	  {port, 8000},
	  {appmods, [{"/web", wf_yaws}
	]}],
	GC = [{cache_refresh_secs, 0}],
	yaws:start_embedded(DocRoot, SC, GC).
