% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_comet_start).
-include ("wf.inc").
-compile(export_all).

render_action(_TriggerPath, _TargetPath, _Record) -> 
	PostbackInfo = action_event:make_postback_info(undefined, comet, undefined, undefined, undefined),
	wf:f("wf_comet_start('~s');", [PostbackInfo]).