-module (action_confirm).
-include ("wf.inc").
-compile(export_all).

render_action(TriggerPath, TargetPath, Record) -> 
	Postback = action_event:make_postback(Record#confirm.postback, undefined, TriggerPath, TargetPath, Record#confirm.delegate, Record#confirm.controls),
	Actions = [wf_render:render_actions(TriggerPath, TargetPath, Record#confirm.actions)],
	wf:f("if (confirm(\"~s\")) { ~s ~s }", [wf_utils:js_escape(Record#confirm.text), Postback, Actions]).
	