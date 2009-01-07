% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_path).
-include ("wf.inc").
-export ([
	get_path/0, push_path/1, pop_path/0,
	to_path/1, 
	to_html_id/1,
	to_js_id/1,
	temp_id/0, is_temp_element/1,
	split_path/1
]).

%%% CURRENT PATH %%%

get_path() -> get(current_path).
push_path(ID) -> put(current_path, [ID|get(current_path)]).
pop_path() -> put(current_path, tl(get(current_path))).


% A Path is of the form [childcontrol, control, parentcontrol, root].
% An htmlID is of the form root__parentcontrol__control__childcontrol
% An atompath is of the form 'root.parentcontrol.control.childcontrol' or 'me.control.childcontrol'

to_path(P) when is_list(P) ->
	case wf:is_string(P) of
		true -> to_absolute_path(split_path(P));
		false -> P
	end;
		
to_path(P) when is_atom(P) ->
	P1 = string:tokens(wf:to_list(P), "."),
	P2 = [wf:to_list(X) || X <- P1],
	P3 = lists:reverse(P2),
	to_path(P3).

to_html_id(P) ->
	P1 = [wf:to_list(X) || X <- lists:reverse(to_path(P))],
	string:join(P1, "__").
	
to_js_id(P) ->
	P1 = [wf:to_list(X) || X <- lists:reverse(to_path(P))],
	string:join(P1, ".").
	
to_absolute_path(RelativePath) -> 
	F = fun(X, Acc) ->
		case X of
			me -> get_path();
			parent -> tl(Acc);
			_ -> [X|Acc]
		end
	end,
	lists:foldr(F, [], RelativePath).

split_path(S) -> lists:reverse(split_path(S, [])).
split_path([], Acc) -> [lists:reverse(Acc)];
split_path([$_,$_|T], Acc) -> [lists:reverse(Acc)|split_path(T, [])];
split_path([H|T], Acc) -> split_path(T, [H|Acc]).

%%% TEMP ELEMENTS %%%

temp_id() ->
	{_, _, C} = now(), 
	"temp" ++ integer_to_list(C).
	
is_temp_element(undefined) -> true;
is_temp_element([P]) -> is_temp_element(P);
is_temp_element(P) -> 
	Name = wf:to_list(P),
	length(Name) > 4 andalso
	lists:nth(1, Name) == $t andalso
	lists:nth(2, Name) == $e andalso
	lists:nth(3, Name) == $m andalso
	lists:nth(4, Name) == $p.