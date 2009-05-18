% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_page_state_bridge, [State]).
-behaviour (keyvalue_bridge).
-include ("simplebridge.hrl").
-include ("wf.inc").
-export ([
	init/1, render/1,
	get/2, put/2, clear/1, clear_all/0
]).

init(Context) -> Context.

render(Context) ->
	% Pickle the state...
	PickledState = wff:pickle(State),
	JS = wff:f("Nitrogen.~s.$set_dom_state=\"~s\";~n", [PickledState]),

	% Queue it as an action...
	NewContext = wff:action(#script { script=JS }, Context),
	NewContext.
	
get(Key, DefaultValue) -> 
	proplists:get_value(Key, State, DefaultValue).

put(Key, Value) ->
	State1 = proplists:delete(Key, State),
	State2 = [{Key, Value}|State1],
	default_page_state_bridge:new(State2).
	
clear(Key) ->
	State1 = proplists:delete(Key, State),
	default_page_state_bridge:new(State1).

clear_all() ->
	default_page_state_bridge:new([]).