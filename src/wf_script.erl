-module (wf_script).
-include ("wf.inc").
-export ([
	add_content_script/1,
	add_dom_script/1,
	add_script/1,
	get_script/0
]).

add_content_script([]) -> ok;
add_content_script(Script) ->
	Script1 = lists:flatten([Script,"\r\n"]),
	put(wf_content_script, [Script1|get(wf_content_script)]).

add_dom_script([]) -> ok;
add_dom_script(Script) ->
	Script1 = lists:flatten([Script,"\r\n"]),
	put(wf_dom_script, [Script1|get(wf_dom_script)]).

add_script([]) -> ok;
add_script(Script) ->
	Script1 = lists:flatten([Script,"\r\n"]),
	put(wf_script, [Script1|get(wf_script)]).

get_script() ->
	ScriptTypes = [wf_content_script, wf_dom_script, wf_script],
	
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
	
	% Add the state variable...
	add_script(wf_state:get_state_script()),
	SecondPass = [lists:reverse(get(X)) || X <- ScriptTypes],
	
	% - FINALLY - 
	% Return the scripts...
	lists:flatten([FirstPass, SecondPass]).
