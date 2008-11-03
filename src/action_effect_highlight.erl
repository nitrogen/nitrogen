% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_effect_highlight).
-include ("wf.inc").
-compile(export_all).

render_action(_TriggerPath, TargetPath, Record) -> 
	Duration = Record#effect_highlight.duration,
	[
		wf:me_var(), 
		wf:f("new Effect.Highlight(obj('~s'), { duration: ~f });", [wf:to_ident(TargetPath), Duration])	
	].