% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_effect_appear).
-include ("wf.inc").
-compile(export_all).

render_action(_TriggerPath, _TargetPath, Record) -> 
	Duration = Record#effect_appear.duration,
	[
		wf:me_var(), 
		"obj('me').setOpacity(0.0);",
		wf:f("new Effect.Appear(obj('me'), { duration: ~f });", [Duration])	
	].