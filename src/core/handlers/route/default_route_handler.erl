% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_route_handler).
-behaviour (route_handler).
-include ("wf.inc").
-include ("simplebridge.hrl").
-export ([
	init/1, 
	finish/1
]).

% TODO 
% This code exposes Nitrogen to a vulnerability where
% someone can hit the server with a bunch of different URLs,
% creating too many atoms and causing Erlang to run out of
% memory.

init(Routes) -> 
	% Get the path...
	RequestBridge = wf_context:request_bridge(),
	Path = RequestBridge:path(),
	
	% Convert the path to a module.
	% If there are no routes defined, then just
	% convert everything without an extension to
	% a module.
	% Otherwise, look through all routes for
	% the first matching route.
	{Module, PathInfo} = case Routes of
		[] -> route_using_extension(Path);
		_ -> route(Path, Routes)
	end,
	
	% TODO - Allow users to supply their own custom 404 module.
	{Module1, PathInfo1} = check_for_404(Module, PathInfo, Path),
					
	wf_context:page_module(Module1),
	wf_context:path_info(PathInfo1),

	{ok, Routes}.
	
finish(State) -> 
	{ok, State}.

%%% PRIVATE FUNCTIONS %%%

% If there is no extension, convert to
% a module name. Otherwise, assume
% it's a static file.
route_using_extension(Path) ->
	case filename:extension(Path) of
		[] ->
			% No extension, it's a module. 
			Path1 = string:strip(Path, both, $/),
			Tokens = string:tokens(Path1, "/"),
			ModuleString = string:join(Tokens, "_"),
			Module = list_to_atom(ModuleString),
			{Module, ""};
		_ ->
			% Serve this up as a static file.
			{static_file, Path}
	end.
	
% Look through all routes for a route that matches
% the specified path. If none are found, then 
% this is a static file.	
route(Path, []) -> 
	{static_file, Path};
	
route(Path, [{Prefix, Module}|Routes]) ->
	case string:str(Path, Prefix) of
		1 -> 
			{Module, string:substr(length(Prefix), Path)};
		_ -> 
			route(Path, Routes)
	end.


check_for_404(static_file, _PathInfo, Path) ->
	{static_file, Path};
		
check_for_404(Module, PathInfo, Path) ->
	case code:ensure_loaded(Module) of
		{module, Module} -> {Module, PathInfo};
		_ -> {file_not_found_page, Path}
	end.
