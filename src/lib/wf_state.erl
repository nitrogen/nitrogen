% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_state).
-include ("wf.inc").
-export ([
	restore_state_from_post/1,
	get_state_script/0, 
	state/1, state/2, clear_state/0
]).

%%% RESTORE STATE %%%

restore_state_from_post(Query) ->
	{value, {_, DomState}} = lists:keysearch("domState", 1, Query),
	case DomState of 
		undefined -> ignore;
		_ ->
			put(wf_state, wf:depickle(DomState))
	end.
	
%%% STATE %%%

%state/1 - Get the value for Key.
state(Key) ->
	case lists:keysearch(Key, 1, get(wf_state)) of
		{value, {Key, Value}} -> Value;
		false -> undefined
	end.

%state/2 - Set the value for key. Use undefined to delete the value.
state(Key, undefined) ->
	case lists:keytake(Key, 1, get(wf_state)) of
		{value, _, State1} -> put(wf_state, State1);
		false -> undefined
	end,	
	undefined;

state(Key, Value) ->
	State = lists:keystore(Key, 1, get(wf_state), {Key, Value}),
	put(wf_state, State),
	Value.

clear_state() ->
	put(wf_state, []).
	
get_state_script() ->
	DomState = get(wf_state),
	wf:f("wf_dom_state = \"~s\";~n", [wf_utils:pickle(DomState)]).