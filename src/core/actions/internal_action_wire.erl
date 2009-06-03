% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (internal_action_wire).
-include ("wf.inc").
-compile(export_all).

% This action is used internally by Nitrogen.
render_action(Record, Context) ->
	DefaultTrigger = Record#wire.trigger,
	DefaultTarget = Record#wire.target,
	Actions = set_paths(DefaultTrigger, DefaultTarget, Record#wire.actions),
	{ok, [Actions, "\n"], Context}.
	
set_paths(_DefaultTrigger, _DefaultTarget, []) -> 
	[];
	
set_paths(DefaultTrigger, DefaultTarget, [H|T]) ->
	[set_paths(DefaultTrigger, DefaultTarget, H)|set_paths(DefaultTarget, DefaultTarget, T)];
	
set_paths(DefaultTrigger, DefaultTarget, Action) when is_tuple(Action) ->
	% If the action doesn't have a target
	Trigger = wff:coalesce([get_trigger(Action), DefaultTrigger]),
	Action1 = set_trigger(Action, Trigger),
	
	Target = wff:coalesce([get_target(Action1), DefaultTarget]),
	_Action2 = set_target(Action1, Target);
	
set_paths(_, _, Other) -> Other.

get_trigger(Action) -> element(3, Action).
set_trigger(Action, Trigger) -> setelement(3, Action, Trigger).
get_target(Action) -> element(4, Action).
set_target(Action, Target) -> setelement(4, Action, Target).

