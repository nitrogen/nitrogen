% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

% Though this is defined as a handler, it is unlikely
% that anyone would want to override the default behaviour. 
% It is defined as a handler simply because it fit well 
% into the existing handler pattern.

-module (default_query_handler).
-behaviour (query_handler).
-include ("wf.inc").
-export ([
	init/1, 
	finish/1,
	get_value/2
]).

init(_State) -> 
	% Get query params and post params
	% from the request bridge...
	RequestBridge = wf_context:request_bridge(),
	QueryParams = RequestBridge:query_params(),
	PostParams = RequestBridge:post_params(),

	% Load into state...
	NewState = QueryParams ++ PostParams,
	{ok, NewState}.
	
finish(_State) -> 
	% Clear out the state.
	{ok, []}.

get_value(Path, State) ->
	% Convert Key to a fuzzy string
	NPath = normalize(Path),
	NPathSize = length(NPath),
	
	% Function to check if our query key ends with NPath.
	F = fun({X, _Y}) -> 
		XPath = string:right(X, NPathSize),		
		("_" ++ NPath == "_" ++ XPath) orelse
		(NPath == X)
	end,
	
	% Filter and return the result.
	% If there is only one, then return the rightmost result, otherwise
	% return a list of results
	Results = [Value || {_Key, Value} <- lists:filter(F, State)],	
	case Results of 
		[] -> undefined;
		[Value] -> Value;
		Values -> Values
	end.

%%% PRIVATE FUNCTIONS %%%
	
normalize(Path) ->
	% Convert to a string and replace periods (.) with underscores (_).
	S = wf:to_list(Path),
	_S1 = replace($., $_, S).
	
replace(Old, New, [Old|T]) -> [New|T];
replace(Old, New, [H|T]) -> [H|replace(Old, New, T)];
replace(_, _, []) -> [].
		
	