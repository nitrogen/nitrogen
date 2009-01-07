% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (validator_custom).
-include ("wf.inc").
-compile(export_all).

render_validator(TriggerPath, TargetPath, Record) -> 
	Validators = wf:state(validators),
	V = {TriggerPath, TargetPath, Record},
	wf:state(validators, lists:delete(V, Validators) ++ [V]),
	[].
