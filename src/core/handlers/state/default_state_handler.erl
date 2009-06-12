% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_state_handler).
-behaviour (state_handler).
-include ("simplebridge.hrl").
-include ("wf.inc").
-export ([
	init/2, 
	finish/2,
	get_state/4, 
	set_state/4, 
	clear/3, 
	clear_all/2
]).

init(Context, State) -> 
	% Deserialize the state from domState.	
	{ok, Context, State}.

finish(Context, State) ->
	{ok, Context, State}.
	
get_state(Key, DefaultValue, _Context, State) -> 
	_Value = proplists:get_value(Key, State, DefaultValue).

set_state(Key, Value, Context, State) ->
	State1 = proplists:delete(Key, State),
	State2 = [{Key, Value}|State1],
	{ok, Context, State2}.
	
clear(Key, Context, State) ->
	State1 = proplists:delete(Key, State),
	{ok, Context, State1}.

clear_all(Context, _State) ->
	init(Context, []).