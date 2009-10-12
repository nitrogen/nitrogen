% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (dynamic_route_handler).
-behaviour (route_handler).
-include ("wf.inc").
-export ([
	init/1, 
	finish/1
]).

init(_) -> 
	% Get the path...
	RequestBridge = wf_context:request_bridge(),
	Path = RequestBridge:path(),
	
	% Convert the path to a module. If there are no routes defined, then just
	% convert everything without an extension to a module.
	% Otherwise, look through all routes for the first matching route.
	{Module, PathInfo} = route(Path),
	{Module1, PathInfo1} = check_for_404(Module, PathInfo, Path),
					
	wf_context:page_module(Module1),
	wf_context:path_info(PathInfo1),

	{ok, undefined}.
	
finish(_State) -> 
	{ok, []}.

%%% PRIVATE FUNCTIONS %%%

% No routes are specified, so do dynamic routing.
% Check if there is an extension, if so, it's static.
% Otherwise, try to load a module according to each part of the path.
% First, cycle through code:all_loaded(). If not there, then check erl_prim_loader:get_file()
% If still not there, then 404.
route("/") -> 
	?PRINT(web_index),
	{web_index, []};
	
route(Path) ->
	IsStatic = (filename:extension(Path) /= []),
	case IsStatic of
		true ->
			% Serve this up as a static file.
			{static_file, Path};
			
		false ->
			Path1 = string:strip(Path, both, $/),
			Tokens = string:tokens(Path1, "/"),
			% Check for a loaded module. If not found, then try to load it.
			case find_loaded_module(Tokens) of
				{Module, PathInfo} -> {Module, PathInfo};
				undefined -> 
					case try_load_module(Tokens) of
						{Module, PathInfo} -> {Module, PathInfo};
						undefined -> {web_404, Path1}
					end
			end
				
	end.
	
find_loaded_module(Tokens) -> find_loaded_module(Tokens, []).	
find_loaded_module([], _ExtraTokens) -> undefined;
find_loaded_module(Tokens, ExtraTokens) ->
	BeamFile = "/" ++ string:join(Tokens, "_") ++ ".beam",
	F = fun({_Module, Path}) -> is_list(Path) andalso string:rstr(Path, BeamFile) /= 0 end,
	case lists:filter(F, code:all_loaded()) of 
		[{Module, _}] -> {Module, string:join(lists:reverse(ExtraTokens), "/")};
		[] -> find_loaded_module(tl(lists:reverse(Tokens)), [hd(lists:reverse(Tokens))|ExtraTokens])
	end.

try_load_module(Tokens) -> try_load_module(Tokens, []).
try_load_module([], _ExtraTokens) -> undefined;
try_load_module(Tokens, ExtraTokens) ->
	BeamFile = string:join(Tokens, "_") ++ ".beam",
	case erl_prim_loader:get_file(BeamFile) of
		{ok, _, _} -> 
			{list_to_atom(string:join(Tokens, "_")), string:join(lists:reverse(ExtraTokens), "/")};
		undefined -> 
			try_load_module(tl(lists:reverse(Tokens)), [hd(lists:reverse(Tokens))|ExtraTokens])
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
