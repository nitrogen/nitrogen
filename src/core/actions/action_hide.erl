% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_hide).
-include ("wf.inc").
-compile(export_all).

render_action(TriggerPath, TargetPath, Record) ->
	Effect = #jquery_effect {
		type=hide,
		effect = Record#hide.effect,
		options = Record#hide.options,
		speed = Record#hide.speed
	},
	action_jquery_effect:render_action(TriggerPath, TargetPath, Effect).