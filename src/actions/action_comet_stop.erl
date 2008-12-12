% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_comet_stop).
-include ("wf.inc").
-compile(export_all).

render_action(_TriggerPath, _TargetPath, _Record) -> 
	"wf_comet_stop();".