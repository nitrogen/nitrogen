% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_event).
-include ("wf.inc").
-compile(export_all).

render_action(Record) -> 
	TriggerPath = Record#event.trigger,
	TargetPath = Record#event.target,
	EventType = Record#event.type, 
	Postback = wf_event:generate_postback_script(Record#event.postback, EventType, TriggerPath, TargetPath, Record#event.delegate),
	SystemPostback = wf_event:generate_system_postback_script(Record#event.postback, EventType, TriggerPath, TargetPath, Record#event.delegate),
	Actions = #wire { trigger=TriggerPath, target=TargetPath, actions=Record#event.actions },

	Script = case EventType of
		
		%%% SYSTEM EVENTS %%%
		% Trigger a system postback immediately...
		system when Record#event.delay == 0 ->
			[
				SystemPostback, Actions
			];
		
		% Trigger a system postback after some delay...
		system ->
			TempID = wf:temp_id(),
			[
				wf:f("document.~s = function() {", [TempID]), SystemPostback, Actions, "};",
				wf:f("setTimeout(\"document.~s(); document.~s=null;\", ~p);", [TempID, TempID, Record#event.delay])
			];
			
		%%% USER EVENTS %%%
		% Run the event when an enter key is hit, such as in an input textbox
		enterkey ->
			[
				wf:f("Nitrogen.$observe_event(obj('~s'), 'keypress', function anonymous(event) {", [wf:to_js_id(TriggerPath)]),
				"if (Nitrogen.$is_enter_key(event)) {", Postback, " ", Actions, "return false; }",
				"});"
			];
		
		% Run the event after a specified amount of time
		timer ->
			TempID = wf:temp_id(),
			[
				wf:f("document.~s = function() {", [TempID]), Postback, Actions, "};",
				wf:f("setTimeout(\"document.~s(); document.~s=null;\", ~p);", [TempID, TempID, Record#event.delay])
			];
		
		% Run some other Javascript event (click, mouseover, mouseout, etc.)
		_ ->
			[
				wf:f("Nitrogen.$observe_event(obj('~s'), '~s', function anonymous(event) {", [wf:to_js_id(TriggerPath), EventType]), 
				Postback, " ", Actions, 
				"});"
			]
			
	end,
	Script.