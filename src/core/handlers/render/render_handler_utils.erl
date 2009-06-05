-module (render_handler_utils).
-include ("wf.inc").
-export ([
	render_elements/2,
	render_actions/2	
]).


% render_elements(Elements, Context) - {ok, NewContext, Html, Script}
% Render the elements in Context#context.elements
% Return the new context and the html and javascript that were produced.
render_elements(Elements, Context) ->
	{ok, HtmlAcc, NewContext} = render_elements(Elements, [], Context),
	{ok, HtmlAcc, NewContext}.

% render_elements(Elements, HtmlAcc, Context) -> {ok, Html, NewContext}.
render_elements(S, HtmlAcc, Context) when S == undefined orelse S == []  ->
	{ok, HtmlAcc, Context};
	
render_elements(S, HtmlAcc, Context) when is_binary(S) orelse ?IS_STRING(S) ->
	{ok, [S|HtmlAcc], Context};

render_elements(Elements, HtmlAcc, Context) when is_list(Elements) ->
	F = fun(X, {ok, HAcc, Cx}) ->
		render_elements(X, HAcc, Cx)
	end,
	{ok, Html, NewContext} = lists:foldl(F, {ok, [], Context}, Elements),
	HtmlAcc1 = [lists:reverse(Html)|HtmlAcc],
	{ok, HtmlAcc1, NewContext};
	
render_elements(Element, HtmlAcc, Context) when is_tuple(Element) ->
	{ok, Html, NewContext} = render_element(Element, Context),
	HtmlAcc1 = [Html|HtmlAcc],
	{ok, HtmlAcc1, NewContext};
	
render_elements(Unknown, _HtmlAcc, _Context) ->
	throw({unanticipated_case_in_render_elements, Unknown}).
	
% This is a Nitrogen element, so render it.
render_element(Element, Context) when is_tuple(Element) ->
	% Get the element's backing module...
	Base = wf_utils:get_elementbase(Element),
	Module = Base#elementbase.module, 

	% Create the element ID...
	CurrentPath = case Base#elementbase.id of
		undefined -> [wf:temp_id()];
		Other -> [wf:to_list(Other)|Context#context.current_path]
	end,
	HtmlID = wf_path:to_html_id(CurrentPath),
	
	% Push the new ID onto the context...
	DomPaths = Context#context.dom_paths,
	Context1 = Context#context { dom_paths=[CurrentPath|DomPaths] },

	case {Base#elementbase.show_if, wf_path:is_temp_element(CurrentPath)} of
		{true, true} -> 			
			% Render the element...
		 	{ok, _Html, _Context2} = call_element_render(Module, HtmlID, Element, Context1);

		{true, false} -> 
			% Update the current path...
			OldPath = Context#context.current_path,
			Context2 = Context1#context { current_path=CurrentPath },
	
			% Render the element...
			{ok, Html, Context3} = call_element_render(Module, HtmlID, Element, Context2),
					
			% Restore the old path...
			Context4 = Context3#context { current_path=OldPath },
			{ok, Html, Context4};
			
		{_, _} -> 
			{ok, [], Context}
	end.
	
% call_element_render(Module, Context, HtmlID, Element) -> {ok, Html, NewContext}.
% Calls the render_element/3 function of an element to turn an element record into
% HTML.
call_element_render(Module, HtmlID, Element, Context) ->
	{module, Module} = code:ensure_loaded(Module),
	{ok, NewElements, Context1} = erlang:apply(Module, render_element, [HtmlID, Element, Context]),

	% Render the new elements into HTML, this could produce even more actions.
	{ok, _Html, _Context2} = render_elements(NewElements, [], Context1).



%%% RENDER ACTIONS %%%

% render_actions(Actions, Context) -> {ok, Script, NewContext}.
render_actions(Actions, Context) ->
	{ok, ScriptAcc, NewContext} = render_actions(Actions, [], Context),
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
			
			% If the target path has changed since last time, then 
			% add some Javascript to set the target path on the client.
			% The #var_me {} action does this.
			{ok, MeScript, Context2} = case (TargetPath1 /= OldPath) of
				true -> 
					MeAction = #var_me { target=TargetPath1 }, 
					render_action(MeAction, Context1);
			
				false -> 
					{ok, [], Context1}
			end,
			
			% Render the action...
			{ok, Script, Context2} = call_action_render(Module, Action1, Context1),
			
			% Restore the old path...
			Context3 = Context2#context { current_path=OldPath },
			{ok, [MeScript, Script], Context3};

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
	
	