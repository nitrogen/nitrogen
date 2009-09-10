% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_function).
-include ("wf.inc").
-compile(export_all).

% The 'function' attribute is an Erlang function of arity 0 that returns {ok, [Elements]}.
%
% Elements can either be a String, a binary, more Nitrogen elements, or a combination thereof. 
%
% Alternatively, the 'function' attribute can be a list
% of functions having the properties above. The first
% one that exists and returns a value (not undefined) will
% be used.

reflect() -> record_info(fields, function).

render_element(_HtmlID, Record) ->
	Functions = lists:flatten([Record#function.function]),
	call_next_function(Functions).
	
call_next_function([]) -> [];
call_next_function([F|Functions]) ->
	% Call the function. If it provides results, then return it, 
	% Otherwise, call the next function.
	case F() of
		undefined -> call_next_function(Functions);
		Elements  -> Elements
	end.