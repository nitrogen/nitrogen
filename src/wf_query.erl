-module (wf_query).
-include ("wf.inc").
-export ([
	q/1,
	prepare_request_query_paths/1
]).
%%% REQUEST QUERY VALUES %%%

prepare_request_query_paths(Query) ->
	% Get all query keys, convert them to reverse sorted paths.
	F = fun(Ident) -> 
		Ident1 = list_to_atom(Ident),
		
		case wf_path:to_paths(Ident1) of
			[] -> 
				% Element not found, just use the supplied ident.
				[Ident1];
			[Path] -> 
				% Found a matching element, use its path.
				Path
		end
	end,
	Paths = [{F(Ident), Value} || {Ident, Value} <- Query],
	put(request_query_paths, Paths).


% q/1 - Lookup any query args that match Path.
q(Partial) when is_atom(Partial) -> 
	% Normalize data for the path search.
	% Split Path into atoms and reverse it.
	Partial1 = [list_to_atom(X) || X <- string:tokens(atom_to_list(Partial), ".")],
	Partial2 = lists:reverse(Partial1),
	q(Partial2);
	
q(Partial) when is_list(Partial) ->	
	% Get the matching paths.
	Paths = wf_utils:path_search(Partial, 1, get(request_query_paths)),

	% Pull out the values.
	[Value || {_, Value} <- Paths].