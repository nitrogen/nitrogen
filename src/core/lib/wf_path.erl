% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_path).
-include ("wf.inc").
-export ([
	split_dom_paths/2,
	normalize_path/2
]).


split_dom_paths(PageName, undefined) ->
	[[wff:to_list(PageName)]];

split_dom_paths(PageName, DomPaths) ->
	BasePath = [wff:to_list(PageName)],
	DomPathList = string:tokens(DomPaths, ","),
	DomPathList1 = [lists:reverse(string:tokens(X, "__")) || X <- DomPathList],
	[BasePath|DomPathList1].


% normalize_path/2 -
% When path is an atom or string, then look for something like this:
%   - me.elementA.elementB
%   - me.parent.elementC
%   - elementA.elementB
% And convert to a path in reverse:
%   - ["elementB", "elementA", "currentelement", "page"]
%   - ["elementC", "parent", "currentelement", "page"]
%   - ["elementB", "elementA"]
normalize_path(Path, Context) when is_atom(Path) orelse ?IS_STRING(Path) ->
	CurrentPath = Context#context.current_path,
	% Convert to reverse sorted list of strings
  % that includes the CurrentPath if possible...
	Path1 = string:tokens(wff:to_list(Path), "."),
	Path2 = case hd(Path1) of
		"me" -> lists:reverse(tl(Path1)) ++ CurrentPath;
		"parent" -> lists:reverse(Path1) ++ CurrentPath;
		_ -> lists:reverse(Path1)
	end,
	
	% Account for any 'parent' tokens.
	Path3 = strip_parents(Path2),
	
	% Path is now a list, so skip to the next clause.
	normalize_path(Path3, Context);

	
% normalize_path/2 - 
% When path is already a list of paths, just pass along to inner_normalize_path/2.
normalize_path(Path, Context) when is_list(Path) ->
	DomPaths = Context#context.dom_paths,
	?PRINT({Path, DomPaths}),
	% Find the one matching dom path.
	case find_matching_dom_path(Path, DomPaths) of
		[] -> throw({no_matching_dom_paths, Path, DomPaths});
		[Match] ->
			% ?PRINT({found_match, Match}),
			Match;
		Matches -> throw({too_many_matching_dom_paths, Path, Matches})
	end.


% strip_parents/1 -
% Path is a reverse sorted list of path strings. 
% Look for any instances of 'parent', and remove the next element
% in the list.
strip_parents([]) -> []; 
strip_parents(["parent", _|T]) -> T;
strip_parents([H|T]) -> [H|strip_parents(T)].
	
% find_matching_dom_path/2 - 
% Path is a reverse sorted list of path strings. Find the one
% path in DomPaths that starts with Path, accounting for
find_matching_dom_path(Path, DomPaths) ->
	% Filter out any paths that are too short...
	Length = length(Path),
	F = fun(X) -> Length =< length(X) end,
	DomPaths1 = lists:filter(F, DomPaths),
	find_matching_dom_path(Path, DomPaths1, 1).

find_matching_dom_path([], _, _) -> [];
find_matching_dom_path(_Path, [], _Pos) -> [];
find_matching_dom_path(Path, DomPaths, Pos) when Pos > length(Path) -> DomPaths; 
find_matching_dom_path(Path, DomPaths, Pos) ->
	Part = lists:nth(Pos, Path),
	F = fun(X) -> Part == lists:nth(Pos, X) end,
	DomPaths1 = lists:filter(F, DomPaths),
	find_matching_dom_path(Path, DomPaths1, Pos + 1).
