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
				trigger = normalize_path(TriggerPath, Context),
				target = TargetPath1 = normalize_path(TargetPath, Context)
			},
			Action1 = wf_utils:replace_with_base(Base1, Action),

			% Update to a new current path...
			Context1 = Context#context { current_path=TargetPath1 },
			
			% Add some Javascript to set the target path on the client.
			ScopeScript = generate_scope_script(Context1),
			
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
	{ok, NewActions, Context1} = erlang:apply(Module, render_action, [Action, Context]),
	{ok, _Script, _Context2} = render_actions(NewActions, [], Context1).
	
% normalize_path/2 -
% When path is an atom or string, then look for something like this:
%   - me.elementA.elementB
%   - me.parent.elementC
%   - elementA.elementB
% And convert to a path in reverse:
%   - ["elementB", "elementA", "currentelement", "page"]
%   - ["elementC", "parent", "currentelement", "page"]
%   - ["elementB", "elementA"]
normalize_path(Path, Context) when is_atom(Path) orelse ?IS_STRING(Path) ->
	CurrentPath = Context#context.current_path,
	
	% Convert to reverse sorted list of strings
  % that includes the CurrentPath if possible...
	Path1 = string:tokens(wff:to_list(Path), "."),
	Path2 = case hd(Path1) of
		me -> lists:reverse(tl(Path1)) ++ CurrentPath;
		parent -> lists:reverse(Path1) ++ CurrentPath;
		_ -> lists:reverse(Path1)
	end,
	
	% Account for any 'parent' tokens.
	Path3 = strip_parents(Path2),
	
	% Path is now a list, so skip to the next clause.
	normalize_path(Path3, Context);

	
% normalize_path/2 - 
% When path is already a list of paths, just pass along to inner_normalize_path/2.
normalize_path(Path, Context) when is_list(Path) ->
	DomPaths = Context#context.dom_paths,
	
	% Find the one matching dom path.
	case find_matching_dom_path(Path, DomPaths) of
		[] -> throw({no_matching_dom_paths, Path, DomPaths});
		[Match] ->
			% ?PRINT({found_match, Match}),
			Match;
		Matches -> throw({too_many_matching_dom_paths, Path, Matches})
	end.


% strip_parents/1 -
% Path is a reverse sorted list of path strings. 
% Look for any instances of 'parent', and remove the next element
% in the list.
strip_parents([]) -> []; 
strip_parents(["parent", _|T]) -> T;
strip_parents([H|T]) -> [H|strip_parents(T)].
	
% find_matching_dom_path/2 - 
% Path is a reverse sorted list of path strings. Find the one
% path in DomPaths that starts with Path, accounting for
find_matching_dom_path(Path, DomPaths) ->
	% Filter out any paths that are too short...
	Length = length(Path),
	F = fun(X) -> Length =< length(X) end,
	DomPaths1 = lists:filter(F, DomPaths),
	find_matching_dom_path(Path, DomPaths1, 1).

find_matching_dom_path([], _, _) -> [];
find_matching_dom_path(_Path, [], _Pos) -> [];
find_matching_dom_path(Path, DomPaths, Pos) when Pos > length(Path) -> DomPaths; 
find_matching_dom_path(Path, DomPaths, Pos) ->
	Part = lists:nth(Pos, Path),
	F = fun(X) -> Part == lists:nth(Pos, X) end,
	DomPaths1 = lists:filter(F, DomPaths),
	find_matching_dom_path(Path, DomPaths1, Pos + 1).

to_js_id(P) ->
	P1 = lists:reverse(P),
	string:join(P1, ".").


generate_scope_script(Context) ->
	Page = Context#context.page_context,
	CurrentID = Page#page_context.name,
	CurrentPath = Context#context.current_path,
	wff:f("Nitrogen.$scope('~s', '~s');~n", [CurrentID, wff:to_js_id(CurrentPath)]).
