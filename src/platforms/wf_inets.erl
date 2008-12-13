% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_inets).
-include ("httpd.hrl").
-export ([do/1]).
	
do(Info) ->
	{Path, _QueryString} = httpd_util:split_path(Info#mod.request_uri),
	Module = wf:path_to_module(Path),
	
	case Path of
		"/" -> do(Info, web_index);
		"/web" ++ _ -> do(Info, Module);
		_ -> {proceed, Info#mod.data}
	end.
	
	
do(Info, Module) ->
	wf_platform:init(inets, Info),
	try wf_handle:handle_request(Module)
	catch Type : Error ->
		io:format("CAUGHT ERROR: ~p-~p~n~p~n", [Type, Error, erlang:get_stacktrace()]),
		{proceed, Info#mod.data}
	end.