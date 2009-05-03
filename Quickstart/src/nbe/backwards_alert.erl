% ./src/nbe/backwards_alert.erl
-module (backwards_alert).
-include ("wf.inc").
-include ("backwards_alert.inc").
-compile(export_all).

render_action(_TriggerPath, _TargetPath, Record) -> 
	Script = "alert(\"~s\".split(' ').reverse().join(' '));",
	Text = wf_utils:js_escape(Record#backwards_alert.text),
	wf:f(Script, [Text]).
	