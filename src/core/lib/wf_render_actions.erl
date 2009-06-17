% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_render_actions).
-include ("wf.inc").
-export ([
	render_actions/2,
	to_js_id/1,
	generate_scope_script/1
]).

%%% RENDER ACTIONS %%%

% render_actions(Actions, Context) -> {ok, Script, NewContext}.
render_actions(Actions, Context) ->
	% Render all actions...
	{ok, ScriptAcc, NewContext} = render_actions(Actions, [], Context),
	
	% Return.
	{ok, ScriptAcc, NewContext}.

% render_actions(Actions, ScriptAcc, Context) -> {ok, Script, NewContext}.
render_actions(S, ScriptAcc, Context) when S == undefined orelse S == []  ->
	{ok, ScriptAcc, Context};
	
render_actions(S, ScriptAcc, Context) when is_binary(S) orelse ?IS_STRING(S) ->
	{ok, [S|ScriptAcc], Context};

render_actions(Actions, ScriptAcc, Context) when is_list(Actions) ->
	F = fun(X, {ok, SAcc, Cx}) ->
		render_actions(X, SAcc, Cx)
	end,
	{ok, Script, NewContext} = lists:foldl(F, {ok, [], Context}, Actions),
	ScriptAcc1 = [lists:reverse(Script)|ScriptAcc],
	{ok, ScriptAcc1, NewContext};
	
render_actions(Action, ScriptAcc, Context) when is_tuple(Action) ->
	{ok, Script, NewContext} = render_action(Action, Context),
	ScriptAcc1 = [Script|ScriptAcc],
	{ok, ScriptAcc1, NewContext};
	
render_actions(Unknown, _ScriptAcc, _Context) ->
	throw({unanticipated_case_in_render_actions, Unknown}).
	
% render_action(Action, Context) -> {ok, Script, NewContext}.
render_action(Action, Context) when is_tuple(Action) ->
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
			OldPath = Context#context.current_path,

			% Get the trigger and target...
			TargetPath = wff:coalesce([Base#actionbase.target, OldPath]),
			TriggerPath = wff:coalesce([Base#actionbase.trigger, TargetPath, OldPath]),

			% Normalize the trigger and target...
			Base1 = Base#actionbase {
				trigger = wf_path:normalize_path(TriggerPath, Context),
				target = TargetPath1 = wf_path:normalize_path(TargetPath, Context)
			},
			Action1 = wf_utils:replace_with_base(Base1, Action),

			% Update to a new current path...
			Context1 = Context#context { current_path=TargetPath1 },
			
			% Add some Javascript to set the target path on the client,
			% but only if this is not a container event. This
			% is kind of a hack to reduce the number of spurious
			% javascript calls to Nitrogen.$scope(...).
			ContainerActions = [action_wire, action_async],
			ScopeScript = case not lists:member(Module, ContainerActions) of
				true -> generate_scope_script(Context1);
				false -> []
			end,
			
			% Render the action...
			{ok, Script, Context2} = call_action_render(Module, Action1, Context1),
			
			% Restore the old path...
			Context3 = Context2#context { current_path=OldPath },

			case Script /= undefined andalso Script/=[] of
				true  -> {ok, [ScopeScript, Script], Context3};
				false -> {ok, [], Context3}
			end;
		_ -> 
			{ok, [], Context}
	end.

% call_action_render(Module, Context, TriggerPath, TargetPath, Action) -> {ok, NewContext, Script}.
% Calls the render_action/4 function of an action to turn an action record into Javascript.
call_action_render(Module, Action, Context) ->
	{module, Module} = code:ensure_loaded(Module),
	{ok, NewActions, Context1} = wf_context:call_with_context(Module, render_action, [Action], Context, true),
	{ok, _Script, _Context2} = render_actions(NewActions, [], Context1).
	
to_js_id(P) ->
	P1 = lists:reverse(P),
	string:join(P1, ".").


generate_scope_script(Context) ->
	Page = Context#context.page_context,
	CurrentID = Page#page_context.name,
	CurrentPath = Context#context.current_path,
	?PRINT(CurrentID),
	?PRINT(CurrentPath),
	Script = wff:f("~nNitrogen.$scope('~s', '~s'); ", [CurrentID, wff:to_js_id(CurrentPath)]),
	Script.
