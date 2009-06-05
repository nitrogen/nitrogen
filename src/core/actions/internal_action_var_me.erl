% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (internal_action_var_me).
-include ("wf.inc").
-compile(export_all).


% This action is used internally by Nitrogen.
render_action(Record, Context) -> 
	CurrentID = Context#context.name,
	CurrentPath = wff:to_js_id(Record#var_me.target),
	Script = wff:f("Nitrogen.$current_id='~s';Nitrogen.$current_path='~s';", [CurrentID, wff:to_js_id(CurrentPath)]),
	{ok, Script, Context}.