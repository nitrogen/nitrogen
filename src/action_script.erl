-module (action_script).
-include ("wf.inc").
-compile(export_all).

render_action(_TriggerPath, _TargetPath, Record) -> 
	[wf:me_var(), Record#script.script, ";"].