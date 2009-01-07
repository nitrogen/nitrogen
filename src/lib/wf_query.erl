% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_query).
-include ("wf.inc").
-export ([
	q/1,
	prepare_request_query_paths/1
]).
%%% REQUEST QUERY VALUES %%%

prepare_request_query_paths(Query) ->
	% Get all query keys, convert them to reverse sorted paths.
	Paths = [{wf_path:to_path(Ident), Value} || {Ident, Value} <- Query],
	put(request_query_paths, Paths).


% q/1 - Lookup any query args that match Path.
q(Partial) when is_atom(Partial) -> 
	% Normalize data for the path search.
	% Split Path into atoms and reverse it.
	Partial1 = string:tokens(atom_to_list(Partial), "."),
	Partial2 = lists:reverse(Partial1),
	q(Partial2);
	
q(Partial) when is_list(Partial) ->	
	% Get the matching paths.
	Partial1 = case wf:is_string(Partial) of
		true -> [Partial];
		false -> Partial
	end,
	
	Paths = wf_utils:path_search(Partial1, 1, get(request_query_paths)),

	% Pull out the values.
	[Value || {_, Value} <- Paths].