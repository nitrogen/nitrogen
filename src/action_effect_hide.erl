-module (action_effect_hide).
-include ("wf.inc").
-compile(export_all).

render_action(_TriggerPath, TargetPath, _Record) -> 
	[wf:me_var(), wf:f("obj('~s').hide();", [wf:to_ident(TargetPath)])].