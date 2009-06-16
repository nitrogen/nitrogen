% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_route_handler).
-behaviour (route_handler).
-include ("wf.inc").
-include ("simplebridge.hrl").
-export ([
	init/2, 
	finish/2
]).

% TODO 
% This code exposes Nitrogen to a vulnerability where
% someone can hit the server with a bunch of different URLs,
% creating too many atoms and causing Erlang to run out of
% memory.

init(Context, State) -> 
	% Get the path.
	Bridge = Context#context.request,
	Path = Bridge:path(),
	
	% Turn the path into a module and pathinfo.
	{Module, PathInfo} = path_to_module(Path),
	{ok, NewContext} = update_context(Module, PathInfo, Context),
	{ok, NewContext, State}.
	
finish(Context, State) -> 
	{ok, Context, State}.

%%% PRIVATE FUNCTIONS %%%

update_context(Module, PathInfo, Context) ->
	Page = Context#context.page_context,
	Page1 = Page#page_context {
		module=Module,
		path_info=PathInfo
	},
	{ok, Context#context { page_context=Page1 }}.

%% path_to_module/1 - Convert a web path to a module.
path_to_module(undefined) -> {web_index, ""};
path_to_module(S) -> 
	case lists:last(S) of
		$/ -> 
			S1 = S ++ "index",
			tokens_to_module(string:tokens(S1, "/"), [], true);
		_ -> 
			tokens_to_module(string:tokens(S, "/"), [], false)
	end.
	
tokens_to_module([], PathInfoAcc, AddedIndex) -> 
	{file_not_found_page, to_path_info(PathInfoAcc, AddedIndex)};
	
tokens_to_module(Tokens, PathInfoAcc, AddedIndex) ->
	try
		% Try to get the name of a module. 
		% TODO - Somehow change this to list_to_existing_atom
		ModuleString = string:join(Tokens, "_"),
		Module = list_to_atom(ModuleString),
		
		% Moke sure the module is loaded.
		{module, Module} = code:ensure_loaded(Module),
		{Module, to_path_info(PathInfoAcc, AddedIndex)}
	catch _ : _ -> 
		% Strip off the last token, and try again.
		LastToken = lists:last(Tokens),
		Tokens1 = lists:reverse(tl(lists:reverse(Tokens))),
		tokens_to_module(Tokens1, [LastToken|PathInfoAcc], AddedIndex)
	end.	
	
chop_last_element(L) -> lists:reverse(tl(lists:reverse(L))).
to_path_info([], _) -> "";
to_path_info(PathInfoAcc, true)  -> string:join(chop_last_element(PathInfoAcc), "/");
to_path_info(PathInfoAcc, false) -> string:join(PathInfoAcc, "/").
