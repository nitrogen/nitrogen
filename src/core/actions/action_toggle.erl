% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_toggle).
-include ("wf.inc").
-compile(export_all).

render_action(Record, Context) ->
	Effect = #jquery_effect {
		type=toggle,
		effect = Record#toggle.effect,
		options = Record#toggle.options,
		speed = Record#toggle.speed
	},
	{ok, Effect, Context}.