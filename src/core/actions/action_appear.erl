% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_appear).
-include ("wf.inc").
-compile(export_all).

render_action(TriggerPath, TargetPath, Record) ->
	Effect = #jquery_effect {
		type=appear,
		speed = Record#appear.speed
	},
	action_jquery_effect:render_action(TriggerPath, TargetPath, Effect).