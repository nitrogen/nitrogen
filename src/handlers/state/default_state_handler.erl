% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_state_handler).
-behaviour (state_handler).
-include ("wf.inc").
-export ([
	init/1, finish/1, get_state/3, set_state/3, clear/2, clear_all/1
]).

init(State) -> 
	% Deserialize the state from domState.
	{ok, State}.

finish(State) ->
	{ok, State}.
	
get_state(Key, DefaultValue, State) -> 
	_Value = proplists:get_value(Key, State, DefaultValue).

set_state(Key, Value, State) ->
	State1 = proplists:delete(Key, State),
	State2 = [{Key, Value}|State1],
	{ok, State2}.
	
clear(Key, State) ->
	State1 = proplists:delete(Key, State),
	{ok, State1}.

clear_all(_State) ->
	init([]).