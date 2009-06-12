% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (validator_custom).
-include ("wf.inc").
-compile(export_all).

render_action(Record, Context) -> 
	TriggerPath= Record#custom.trigger,
	TargetPath = Record#custom.target,
	Validators = state_handler:get_state(validators, [], Context),
	V = {TriggerPath, TargetPath, Record},
	{ok, Context1} = state_handler:set_state(validators, lists:delete(V, Validators) ++ [V], Context),
	{ok, [], Context1}.
