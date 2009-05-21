% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_state_handler).
-behaviour (state_handler).
-include ("simplebridge.hrl").
-include ("wf.inc").
-export ([
	init/1, 
	finish/2,
	get/4, 
	put/4, 
	clear/3, 
	clear_all/2
]).

init(Context) -> 
	{ok, Context, []}.

finish(Context, State) ->
	% % Pickle the state...
	% PickledState = wff:pickle(State),
	% JS = wff:f("Nitrogen.~s.$set_dom_state=\"~s\";~n", [PickledState]),
	% 
	% % Queue it as an action...
	% NewContext = wff:action(#script { script=JS }, Context),
	% NewContext.
	{ok, Context, State}.
	
get(Context, State, Key, DefaultValue) -> 
	Value = proplists:get_value(Key, State, DefaultValue),
	{ok, Context, State, Value}.

put(Context, State, Key, Value) ->
	State1 = proplists:delete(Key, State),
	State2 = [{Key, Value}|State1],
	{ok, Context, State2}.
	
clear(Context, State, Key) ->
	State1 = proplists:delete(Key, State),
	{ok, Context, State1}.

clear_all(Context, State) ->
	init(Context).