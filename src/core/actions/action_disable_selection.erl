% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_disable_selection).
-include ("wf.inc").
-compile(export_all).

render_action(_Record, Context) -> 
	Script = "Nitrogen.$disable_selection(obj(me));",
	{ok, Script, Context}.