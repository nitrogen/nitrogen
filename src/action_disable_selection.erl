-module (action_disable_selection).
-include ("wf.inc").
-compile(export_all).

render_action(_TriggerPath, TargetPath, _Record) -> 
	[
		wf:me_var(), 
		wf:f("wf_disable_selection(obj('~s'));", [wf:to_ident(TargetPath)])	
	].