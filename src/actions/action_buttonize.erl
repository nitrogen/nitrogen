% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_buttonize).
-include ("wf.inc").
-compile(export_all).

render_action(TriggerPath, TargetPath, _Record) -> 
	Actions = [
		#event { type=mouseover, actions=#add_class { class=hover } },
		#event { type=mouseout, actions=#remove_class { class=hover } },
		#event { type=mousedown, actions=#add_class { class=clicked } },
		#event { type=mouseup, actions=#remove_class { class=clicked } }
	],
	wf_render:render_actions(TriggerPath, TargetPath, Actions).
