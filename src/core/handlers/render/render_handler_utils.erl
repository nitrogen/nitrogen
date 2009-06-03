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

	% Create the ID...
	ID = case Base#elementbase.id of
		undefined -> wf:temp_id();
		Other -> Other
	end,
	case {Base#elementbase.show_if, wf_path:is_temp_element(ID)} of
		{true, true} -> 			
			HtmlID = wf_path:to_html_id(ID),
		 	{ok, _Html, _Context1} = call_element_render(Module, HtmlID, Element, Context);

		{true, false} -> 
			% Update the current path...
			OldPath = Context#context.current_path,
			CurrentPath = [ID|OldPath],
			Context1 = Context#context { current_path=CurrentPath },
			HtmlID = wf_path:to_html_id(CurrentPath),
			
			% Render the element...
			{ok, Html, Context2} = call_element_render(Module, HtmlID, Element, Context1),
					
			% Restore the old path...
			Context3 = Context2#context { current_path=OldPath },
			{ok, Html, Context3};
			
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
			% Update the current path...
			TargetPath = Base#actionbase.target,
			OldPath = Context#context.current_path,
			Context1 = Context#context { current_path=TargetPath },
			
			% If the target path has changed since last time, then 
			% add some Javascript to set the target path on the client.
			% The #var_me {} action does this.
			{ok, MeScript, Context2} = case (TargetPath /= OldPath) of
				true -> 
					MeAction = #var_me { target=TargetPath }, 
					render_action(MeAction, Context1);
			
				false -> 
					{ok, [], Context1}
			end,
			
			% Render the action...
			{ok, Script, Context2} = call_action_render(Module, Action, Context1),
			
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