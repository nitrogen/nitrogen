% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_convert).
-export ([
	clean_lower/1,
	to_list/1, 
	to_atom/1, 
	to_binary/1, 
	to_integer/1,
	html_encode/1, html_encode/2
]).


%%% CONVERSION %%%

clean_lower(L) -> string:strip(string:to_lower(to_list(L))).

to_list(undefined) -> [];
to_list(L) when is_list(L) -> inner_to_list(lists:flatten(L));
to_list(A) -> inner_to_list(A).
inner_to_list(A) when is_atom(A) -> atom_to_list(A);
inner_to_list(B) when is_binary(B) -> binary_to_list(B);
inner_to_list(I) when is_integer(I) -> integer_to_list(I);
inner_to_list(L) when is_list(L) -> L.

to_atom(A) when is_atom(A) -> A;
to_atom(B) when is_binary(B) -> to_atom(binary_to_list(B));
to_atom(I) when is_integer(I) -> to_atom(integer_to_list(I));
to_atom(L) when is_list(L) -> list_to_atom(binary_to_list(list_to_binary(L))).

to_binary(A) when is_atom(A) -> to_binary(atom_to_list(A));
to_binary(B) when is_binary(B) -> B;
to_binary(I) when is_integer(I) -> to_binary(integer_to_list(I));
to_binary(L) when is_list(L) -> list_to_binary(L).

to_integer(A) when is_atom(A) -> to_integer(atom_to_list(A));
to_integer(B) when is_binary(B) -> to_integer(binary_to_list(B));
to_integer(I) when is_integer(I) -> I;
to_integer(L) when is_list(L) -> list_to_integer(L).

%%% HTML ENCODE %%%

html_encode(L, false) -> wf:to_list(lists:flatten([L]));
html_encode(L, true) -> html_encode(wf:to_list(lists:flatten([L]))).	
html_encode([]) -> [];
html_encode([H|T]) ->
	case H of
		$\s -> "&nbsp;" ++ html_encode(T);
		$\t -> "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" ++ html_encode(T);
		$< -> "&lt;" ++ html_encode(T);
		$> -> "&gt;" ++ html_encode(T);
		$" -> "&quot;" ++ html_encode(T);
		$& -> "&amp;" ++ html_encode(T);
		$\n -> "<br>" ++ html_encode(T);
		_ -> [H|html_encode(T)]
	end.