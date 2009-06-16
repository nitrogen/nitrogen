% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_function).
-include ("wf.inc").
-compile(export_all).

% The 'function' attribute is an Erlang function that either:
%
%	- Takes no parameters and returns [Elements].
% - Takes one parameter which is set to Context, 
%     and returns {ok, [Elements], NewContext}.
%
% Elements can either be a String, a binary, more Nitrogen elements, or a combination. 
%
% Alternatively, the 'function' attribute can be a list
% of functions having the properties above. The first
% one that exists and returns a value (not undefined) will
% be used.

reflect() -> record_info(fields, function).

render_element(_ControlID, Record, Context) ->
	Functions = lists:flatten([Record#function.function]),
	call_next_function(Functions, Context).
	
call_next_function([], Context) -> {ok, [], Context};
call_next_function([F|Functions], Context) ->
	% Normalize the function so that it has arity of 1
	% and returns the right format.
	{arity, Arity} = erlang:fun_info(F, arity),
	NewFunction = case Arity of
		0 -> wrap_function(F);
		1 -> F;
		_ -> throw({incorrect_arity_in_function, F})
	end,
	
	% Call the function. If it provides results, then return it, 
	% Otherwise, call the next function.
	case NewFunction(Context) of
		{ok, Elements, NewContext} -> {ok, Elements, NewContext};
		undefined -> call_next_function(Functions, Context)
	end.
	
% Wrap a function of arity 0 so that it looks like arity 1.
% In this case, the function will expect that the Context
% has been loaded into the process dictionary.
wrap_function(Function) ->
	fun(Context) ->
		put(context, Context),
		Elements = Function(),
		{ok, Elements, get(context)}
	end.