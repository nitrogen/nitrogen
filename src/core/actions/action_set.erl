% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_set).
-include ("wf.inc").
-compile(export_all).

% This action is used internally by Nitrogen.
render_action(Record, Context) ->
	Value = wf_utils:js_escape(wf:to_list(Record#set.value)),
	Script = wf:f("Nitrogen.$set_value('me', \"~s\");", [Value]),
	{ok, Script, Context}.

set(Element, Value, Context) ->
	{ok, _NewContext} = wff:wire(Element, #set { value=Value }, Context).


% TODO - not sure if anyone uses this anymore.
% bind(T, Record) when is_tuple(T), is_tuple(Record) -> 
% 	bind(2, T, Record).
% 	
% bind(Pos, T, Record) when Pos =< size(T) ->
% 	case element(Pos, T) of 
% 		undefined -> 
% 			ok;
% 		Element ->
% 			set(Element, element(Pos, Record))
% 	end,
% 	bind(Pos + 1, T, Record);
% 	
% bind(_, _, _) -> ok.
% 
% 
% reverse_bind(T) when is_tuple(T) ->
% 	Head = element(1, T),
% 	Tail = lists:duplicate(size(T) - 1, undefined),
% 	Record = list_to_tuple([Head|Tail]),
% 	reverse_bind(T, Record).
% 	
% reverse_bind(T, Record) when is_tuple(T), is_tuple(Record) -> 
% 	reverse_bind(2, T, Record). 
% 
% reverse_bind(Pos, T, Record) when Pos =< size(T) ->
% 	Value = case element(Pos, T) of
% 		undefined -> 
% 			element(Pos, Record);
% 		Element ->
% 			[X] = wf:q(Element),
% 			X
% 	end,
% 	reverse_bind(Pos + 1, T, setelement(Pos, Record, Value));
% 	
% reverse_bind(_, _, Record) -> Record.