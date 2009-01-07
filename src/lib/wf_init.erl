% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_init).
-include ("wf.inc").
-export ([init/0]).

init() -> 
	% wf_cache:init(),
	atomize_pages(),
	ok.


%% atomize_pages/1 - Create an atom for each possible Nitrogen page.
%% Necessary because when we convert a page to a path, we use list_to_existing_atom,
%% so this is where we "initialize" the atoms.
atomize_pages() ->
	% Get a list of bin directories that are not Erlang lib directories.
	LibPath = code:lib_dir(),
	AllPaths = code:get_path(),
	F = fun(Path) -> false==lists:prefix(LibPath, Path) end,
	LocalPaths = lists:filter(F, AllPaths),
	[atomize_pages_in_path(X) || X <- LocalPaths].

atomize_pages_in_path(Path) ->	
	% Get all of the .beam files in those directories, and make atoms.
	BeamFiles = filelib:wildcard("*.beam", Path),
	[list_to_atom(filename:rootname(X)) || X <- BeamFiles].