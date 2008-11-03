-module (validator_custom).
-include ("wf.inc").
-compile(export_all).

render_validator(TriggerPath, TargetPath, Record) -> 
	Validators = wf:state(validators),
	V = {TriggerPath, TargetPath, Record},
	wf:state(validators, lists:delete(V, Validators) ++ [V]),
	[].
