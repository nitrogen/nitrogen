-module (action_event).
-include ("wf.inc").
-compile(export_all).

render_action(TriggerPath, TargetPath, Record) -> 
	EventType = Record#event.type, 
	Postback = make_postback(Record#event.postback, EventType, TriggerPath, TargetPath, Record#event.delegate, Record#event.controls),
	Actions = [wf_render:render_actions(TriggerPath, TargetPath, Record#event.actions)],

	case EventType of
		enterkey ->
			[
				wf:f("obj('~s').observe('keypress', function anonymous(event) {", [wf:to_ident(TriggerPath)]),
				wf:f("if (wf_is_enter_key(event)) { ~s ~s; Event.stop(event); }", [Postback, Actions]),
				wf:f("});\r\n")
			];
		
		_ when EventType == interval orelse EventType == continuation ->
			wf:f("setTimeout(\"~s ~s\", ~p);", [wf_utils:js_escape(Postback), wf_utils:js_escape(Actions), Record#event.interval]);
			
		_ ->
			[
				wf:f("obj('~s').observe('~s', function anonymous(event) { ~s ~s });\r\n", [wf:to_ident(TriggerPath), EventType, Postback, Actions])
			]
	end.
	
make_postback(Postback, EventType, TriggerPath, TargetPath, Delegate, Controls) ->
	case Postback of
		undefined -> [];
		Tag ->
			EventInfo = {EventType, TriggerPath, TargetPath, Tag, Delegate},
			S = string:join([["obj('", atom_to_list(X), "')"] || X <- Controls], ", "),
			wf:f("wf_queue_event('~s', '~s', new Array(~s));", [wf:to_ident(TriggerPath), wf_utils:pickle(EventInfo), S])
	end.