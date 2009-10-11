% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_render_actions).
-include ("wf.inc").
-export ([
	render_actions/1,
	to_js_id/1,
	generate_scope_script/0
]).

%%% RENDER ACTIONS %%%

% render_actions(Actions) -> {ok, Script}.
render_actions(Actions) ->
	% Render all actions...
	{ok, ScriptAcc} = render_actions(Actions, []),
	
	% Return.
	{ok, ScriptAcc}.

% render_actions(Actions, ScriptAcc) -> {ok, Script}.
render_actions(S, ScriptAcc) when S == undefined orelse S == []  ->
	{ok, ScriptAcc};
	
render_actions(S, ScriptAcc) when is_binary(S) orelse ?IS_STRING(S) ->
	{ok, [S|ScriptAcc]};

render_actions(Actions, ScriptAcc) when is_list(Actions) ->
	F = fun(X, {ok, SAcc}) ->
		render_actions(X, SAcc)
	end,
	{ok, Script} = lists:foldl(F, {ok, []}, Actions),
	ScriptAcc1 = [lists:reverse(Script)|ScriptAcc],
	{ok, ScriptAcc1};
	
render_actions(Action, ScriptAcc) when is_tuple(Action) ->
	{ok, Script} = render_action(Action),
	ScriptAcc1 = [Script|ScriptAcc],
	{ok, ScriptAcc1};
	
render_actions(Unknown, _ScriptAcc) ->
	throw({unanticipated_case_in_render_actions, Unknown}).
	
% render_action(Action) -> {ok, Script}.
render_action(Action) when is_tuple(Action) ->
	Base = wf_utils:get_actionbase(Action),
	Module = Base#actionbase.module, 
	
	% Verify that this is an action...
	case Base#actionbase.is_action == is_action of
		true -> ok;
		false -> throw({not_an_action, Action})
	end,

	% Render...
	case Base#actionbase.show_if of 
		true -> 
			% Save the current path...
			OldPath = wf_context:current_path(),

			% Get the trigger and target...
			TargetPath = wf:coalesce([Base#actionbase.target, OldPath]),
			TriggerPath = wf:coalesce([Base#actionbase.trigger, TargetPath, OldPath]),

			% Normalize the trigger and target...
			Base1 = Base#actionbase {
				trigger = wf_path:normalize_path(TriggerPath),
				target = TargetPath1 = wf_path:normalize_path(TargetPath)
			},
			Action1 = wf_utils:replace_with_base(Base1, Action),

			% Update to a new current path...
			wf_context:current_path(TargetPath1),
			
			% Add some Javascript to set the target path on the client,
			% but only if this is not a container event. This
			% is kind of a hack to reduce the number of spurious
			% javascript calls to Nitrogen.$scope(...).
			ContainerActions = [action_wire, action_comet],
			ScopeScript = case not lists:member(Module, ContainerActions) of
				true -> generate_scope_script();
				false -> []
			end,
			
			% Render the action...
			{ok, Script} = call_action_render(Module, Action1),
			
			% Restore the old path...
			wf_context:current_path(OldPath),

			case Script /= undefined andalso Script/=[] of
				true  -> {ok, [ScopeScript, Script]};
				false -> {ok, []}
			end;
		_ -> 
			{ok, []}
	end.

% call_action_render(Module, TriggerPath, TargetPath, Action) -> {ok, Script}.
% Calls the render_action/4 function of an action to turn an action record into Javascript.
call_action_render(Module, Action) ->
	{module, Module} = code:ensure_loaded(Module),
	NewActions = Module:render_action(Action),
	{ok, _Script} = render_actions(NewActions, []).
	
to_js_id(P) ->
	P1 = lists:reverse(P),
	string:join(P1, ".").


generate_scope_script() ->
	CurrentPath = wf_context:current_path(),
	Script = wf:f("~nNitrogen.$scope('~s'); ", [wf:to_js_id(CurrentPath)]),
	Script.
