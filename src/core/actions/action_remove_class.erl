% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_remove_class).
-include ("wf.inc").
-compile(export_all).

render_action(TriggerPath, TargetPath, Record) ->
	Effect = #jquery_effect {
		type=remove_class,
		class = Record#remove_class.class,
		speed = Record#remove_class.speed
	},
	action_jquery_effect:render_action(TriggerPath, TargetPath, Effect).