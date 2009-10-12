% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (named_route_handler).
-behaviour (route_handler).
-include ("wf.inc").
-export ([
	init/1, 
	finish/1
]).

init(undefined) -> init([]);
init(Routes) -> 
	% Get the path...
	RequestBridge = wf_context:request_bridge(),
	Path = RequestBridge:path(),
	
	% Match to the longest possible route.
	{Module, PathInfo} = route(Path, Routes),
	{Module1, PathInfo1} = check_for_404(Module, PathInfo, Path),
	wf_context:page_module(Module1),
	wf_context:path_info(PathInfo1),
	{ok, Routes}.
	
finish(_State) -> 
	{ok, []}.

%%% PRIVATE FUNCTIONS %%%

% Look through all routes for a route that matches
% the specified path. If none are found, then 
% this is a static file. If more than one route are
% found, takes the longest.
route(Path, Routes) ->
	% Returns {SizeOfMatch, Prefix, Module}
	F = fun(Prefix, Module) ->
		case string:str(Path, Prefix) of
			1 -> {length(Prefix), Prefix, Module};
			_ -> not_found
		end
	end,
	Matches = [F(Prefix, Module) || {Prefix, Module} <- Routes],
	Matches1 = lists:reverse(lists:sort([X || X <- Matches, X /= not_found])),

	case Matches1 of
		[] ->
			{static_file, Path};
		[{_, Prefix, Module}|_] ->
			{Module, string:substr(Path, length(Prefix))}
	end.

check_for_404(static_file, _PathInfo, Path) ->
	{static_file, Path};
		
check_for_404(Module, PathInfo, Path) ->
	% Make sure the requested module is loaded. If it
	% is not, then try to load the web_404 page. If that
	% is not available, then default to the 'file_not_found_page' module.
	case code:ensure_loaded(Module) of
		{module, Module} -> {Module, PathInfo};
		_ -> 
			case code:ensure_loaded(web_404) of
				{module, web_404} -> {web_404, Path};
				_ -> {file_not_found_page, Path}
			end
	end.
