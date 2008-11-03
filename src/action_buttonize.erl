% Nitrogen Web Framework for Erlang
% Copyright (c) 2008 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_buttonize).
-include ("wf.inc").
-compile(export_all).

render_action(TriggerPath, TargetPath, _Record) -> 
	Actions = [
		#event { type=mouseover, actions=wf:f("obj('~s').addClassName('hover')", [TargetPath]) },
		#event { type=mouseout, actions=wf:f("obj('~s').removeClassName('hover')", [TargetPath]) },
		#event { type=mousedown, actions=wf:f("obj('~s').addClassName('clicked')", [TargetPath]) },
		#event { type=mouseup, actions=wf:f("obj('~s').removeClassName('clicked')", [TargetPath]) }
	],
	wf_render:render_actions(TriggerPath, TargetPath, Actions).
