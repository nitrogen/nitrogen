% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_effect_blindup).
-include ("wf.inc").
-compile(export_all).

render_action(_TriggerPath, TargetPath, Record) -> 
	Duration = Record#effect_blindup.duration,
	[
		wf:me_var(), 
		wf:f("new Effect.BlindUp(obj('~s'), { duration: ~f });", [wf:to_js_id(TargetPath), Duration])	
	].