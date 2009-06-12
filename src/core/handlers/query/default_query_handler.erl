% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_query_handler).
-behaviour (query_handler).
-include ("wf.inc").
-export ([
	init/2, 
	finish/2,
	get_value/3
]).

init(Context, _State) -> 
	% Get query params and post params
	% from the request bridge...
	Bridge = Context#context.request,
	QueryParams = Bridge:query_params(),
	PostParams = Bridge:post_params(),

	% Load into state...
	NewState = QueryParams ++ PostParams,
	{ok, Context, NewState}.
	
finish(Context, _State) -> 
	% Clear out the state.
	{ok, Context, []}.

get_value(Path, _Context, State) ->
	% Convert Key to a fuzzy string
	NPath = normalize(Path),
	NPathSize = length(NPath),
	
	% Function to check if our query key ends with NPath.
	F = fun({X, _Y}) -> 
		NPath == string:right(X, NPathSize)
	end,
	
	% Filter and return the result. There should only be one.
	Results = lists:filter(F, State),	
	_Value = case Results of 
		[] -> undefined;
		[{_X, Y}] -> Y;
		_ -> throw({too_many_matching_parameters, Path})
	end.

%%% PRIVATE FUNCTIONS %%%
	
normalize(Path) ->
	% Convert to a string and replace periods (.) with underscores (_).
	S = wff:to_list(Path),
	_S1 = replace($., $_, S).
	
replace(Old, New, [Old|T]) -> [New|T];
replace(Old, New, [H|T]) -> [H|replace(Old, New, T)];
replace(_, _, []) -> [].
		
	