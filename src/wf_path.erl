% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_path).
-include ("wf.inc").
-export ([
	register_path/2,
	restore_paths_from_post/1,
	
	to_path/1, 
	to_paths/1,
	to_ident/1,
	js_id/1, parent_js_id/1, html_id/1,
	temp_id/0, is_temp_element/1	
]).

register_path(Ident, Path) ->
	DomPaths = get(wf_paths),
	put(wf_paths, lists:keystore(Ident, 1, DomPaths, {Ident, Path})),
	ok.

restore_paths_from_post(Query) ->	
	% Restore paths...
	{value, {_, DomPaths}} = lists:keysearch("domPaths", 1, Query),
	case DomPaths of
		undefined -> ignore;
		_ ->
			L1 = string:tokens(DomPaths, "|"),
			put(wf_paths, [wf:depickle(X) || X <- L1])
	end,
	register_path(page, [page]).

%%% CONTROL IDS %%%

js_id(Path) when is_atom(Path) -> "dom_root." ++ atom_to_list(Path);
js_id(Path) -> "dom_root." ++ lists:flatten(string:join([wf:to_list(X) || X <- lists:reverse(Path)], ".")).

parent_js_id(Path) -> js_id(tl(Path)).

html_id(Path) when is_atom(Path) -> Path;
html_id(Path) -> list_to_atom(string:join([atom_to_list(X) || X <- lists:reverse(Path)], "::")).

%%% NORMALIZE PATHS %%%
% If it's an atom that starts with 'me', then turn it from relative to absolute.
% If it's just an atom, then conduct a path search. Return if only one is found, throw exception otherwise.
% If it's a string, then looking it up in our list of elements.

to_ident(P) ->
	P1 = to_path(P),
	case is_temp_element(P1) of
		true -> 
			html_id(P1);
			
		false ->
			Paths = get(wf_paths),
			case lists:keysearch(P1, 2, Paths) of
				{value, {Ident, _}} -> 
					Ident;
				false ->
					?LOG("No matching paths for ident '~p'. Double check your code.", [P]),
					wf:break(),
					erlang:error({no_matching_paths_for_ident, P})
			end
	end.
	
to_path(P) ->
	Paths = to_paths(P),
	case Paths of 
		[] -> 
			?LOG("No matching paths for path '~p'. Double check your code.", [P]),
			wf:break(),
			erlang:error({no_matching_paths_for_path, P});
	
		[Path] -> 
			Path;
	
		_ ->
			?LOG("More than one path for path '~p'. Double check your code.", [P]),
			wf:break(),
			erlang:error({too_many_paths_for_path, P})
	end.

to_paths(P) when is_atom(P) ->
	% P is an atom, such as page.control1.controlA or me.parent.controlA
	% Break up the atom into its parts and recurse.
	P1 = [list_to_atom(X) || X <- string:tokens(atom_to_list(P), ".")],
	case hd(P1) of
		me -> 
			% Resolve the relative path.
			[combine_paths(get(current_path), P1)];
			
		_ ->	
			% Do a fuzzy search.
			to_paths(lists:reverse(P1))
	end;

to_paths(P) when is_list(P) ->
	case is_temp_element(P) of
		true -> 
			[P];
		false ->
			case P of
				[Ident] -> 
					ident_to_paths(Ident);
				_ ->
					partial_path_to_paths(P)
			end
	end;

to_paths(P) ->
	% P is some type we haven't accounted for, so error.
	?LOG("Could not handle path: ~p", [P]),
	wf:break(),
	erlang:error({could_not_handle_path, P}).	

ident_to_paths(Ident) ->
	% P is either the ident of a control (such as c26) or a partial path. Try
	% looking it up as an ident, and if that fails, then look it up as a 
	% partial path.
	Paths = get(wf_paths),
	case lists:keysearch(Ident, 1, Paths) of
		{value, {_, Path}} -> 
			[Path];
		false -> 
			partial_path_to_paths([Ident])
	end.
			
partial_path_to_paths(P) ->
	% P is either all or part of a path, such as [controlA, control1, page] or [controlA, control1]
	% Do a fuzzy search to find a match. Error if either no matches or too many matches are found.
	Paths = get(wf_paths),
	Matches = wf_utils:path_search(P, 2, Paths),
	[Path || {_Ident, Path} <- Matches].

combine_paths(Path, [me|T]) -> combine_paths(Path, T);
combine_paths([_|T1], [parent|T2]) -> combine_paths(T1, T2);
combine_paths(Path1, Path2) -> lists:reverse(Path2) ++ Path1.

%%% TEMP ELEMENTS %%%

temp_id() ->
	{_, _, C} = now(), 
	list_to_atom("temp" ++ integer_to_list(C)).
	
is_temp_element(undefined) -> true;
is_temp_element([P]) -> is_temp_element(P);
is_temp_element(P) when is_atom(P) -> 
	Name = atom_to_list(P),
	length(Name) > 4 andalso
	lists:nth(1, Name) == $t andalso
	lists:nth(2, Name) == $e andalso
	lists:nth(3, Name) == $m andalso
	lists:nth(4, Name) == $p;

is_temp_element(_) -> false.