% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_script).
-include ("wf.inc").
-export ([
	add_content_script/1,
	add_script/1,
	get_script/0,
	get_script/1
]).

add_content_script([]) -> ok;
add_content_script(Script) ->
	Script1 = lists:flatten([Script,"\r\n"]),
	put(wf_content_script, [Script1|get(wf_content_script)]).

add_script([]) -> ok;
add_script(Script) ->
	Script1 = lists:flatten([Script,"\r\n"]),
	put(wf_script, [Script1|get(wf_script)]).

get_script() -> get_script(true).
get_script(IncludeStateScript) ->
	ScriptTypes = [wf_content_script, wf_script],
	
	% - FIRST PASS - 
	% Gather first-pass scripts, then clear for a second pass.
	% These scripts are from any rendering done in the main() method.
	FirstPass = [lists:reverse(get(X)) || X <- ScriptTypes],
	[put(X, []) || X <- ScriptTypes],
	
	% - SECOND PASS - 
	% Process queued updates...
	DoUpdate = fun({TargetPath, Terms, JSFormatString}) ->
		TargetPath1 = wf_path:to_path(TargetPath),
		put(current_path, TargetPath1),
		Html = wf:render(Terms),
		Script = [wf:me_var(), wf:f(JSFormatString, [wf_utils:js_escape(Html)])],
		add_content_script(Script)
	end,
	UpdateQueue = get(wf_update_queue),
	[DoUpdate(X) || X <- lists:reverse(UpdateQueue)],	

	% Process queued actions...
	DoWire = fun({TriggerPath, TargetPath, Actions}) ->
		Script = wf_render:render_actions(TriggerPath, TargetPath, Actions),
		add_script(Script)
	end,
	ActionQueue = get(wf_action_queue),
	[DoWire(X) || X <- lists:reverse(ActionQueue)],
	SecondPass = [lists:reverse(get(X)) || X <- ScriptTypes],
	
	% - FINALLY - 
	% Return the scripts...
	WFStateScript = case IncludeStateScript of
		true -> wf_state:get_state_script();
		false -> []
	end,
	lists:flatten([WFStateScript, FirstPass, SecondPass]).
