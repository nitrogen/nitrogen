% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_effect).
-include ("wf.inc").
-compile(export_all).

render_action(Record, Context) ->
	Effect = #jquery_effect {
		trigger=Record#effect.trigger,
		target=Record#effect.target,
		type=effect,
		effect = Record#effect.effect,
		options = Record#effect.options,
		speed = Record#effect.speed
	},
	{ok, Effect, Context}.