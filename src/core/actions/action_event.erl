% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_event).
-include ("wf.inc").
-compile(export_all).

render_action(Record, Context) -> 
	TriggerPath = Record#event.trigger,
	TargetPath = Record#event.target,
	EventType = Record#event.type, 
	Postback = wf_event:generate_postback_script(Record#event.postback, EventType, TriggerPath, TargetPath, Record#event.delegate, Context),
	Actions = #wire { trigger=TriggerPath, target=TargetPath, actions=Record#event.actions },

	Script = case EventType of
		enterkey ->
			[
				wff:f("Nitrogen.$observe_event(obj('~s'), 'keypress', function anonymous(event) {\r\n", [wff:to_js_id(TriggerPath)]),
				"if (Nitrogen.$is_enter_key(event)) {\r\n", Postback, " ", Actions, "return false; }",
				"});\r\n"
			];
		
		immediate ->
			[
				Postback, Actions
			];
		
		timer ->
			TempID = wff:temp_id(),
			[
				wff:f("document.~s = function() {\r\n", [TempID]), Postback, Actions, "};\r\n",
				wff:f("setTimeout(\"document.~s(); document.~s=null;\", ~p);\r\n", [TempID, TempID, Record#event.delay])
			];
			
		_ ->
			[
				wff:f("Nitrogen.$observe_event(obj('~s'), '~s', function anonymous(event) {\r\n", [wff:to_js_id(TriggerPath), EventType]), 
				Postback, " ", Actions, 
				"});\r\n"
			]
	end,
	{ok, Script, Context}.