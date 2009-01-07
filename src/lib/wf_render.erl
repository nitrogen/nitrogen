% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_render).
-include ("wf.inc").
-export ([
	me_var/0,
	
	render/1,
	render_actions/3,
	
	update/2,
	insert_top/2,
	insert_bottom/2,
	
	wire/1, wire/2, wire/3
]).

me_var() -> 
	Path = get(current_path),
	["wf_current_path='", wf_path:to_js_id(Path), "'; "].

render(undefined) -> "";
render(Term) when is_binary(Term) -> Term;
render(Terms=[H|_]) when is_list(Terms), is_integer(H) -> Terms;
render(Terms) when is_list(Terms) ->[render(X) || X <- Terms];
render(Term) when is_tuple(Term) ->
	Base = wf_utils:get_elementbase(Term),
	Module = Base#elementbase.module, 

	% Load vars...
	ID = case Base#elementbase.id of
		undefined -> wf:temp_id();
		Other -> Other
	end,
	
	Response = case {Base#elementbase.show_if, wf_path:is_temp_element(ID)} of
		{true, true} -> 			
			% Wire actions and render the control.
			HtmlID = wf_path:to_html_id(ID),
			wf:wire(HtmlID, HtmlID, Base#elementbase.actions),
			lists:flatten([Module:render(HtmlID, Term)]);

		{true, false} -> 
			% Set the new path...
			wf_path:push_path(ID),

			% Wire actions and render the control.
			HtmlID = wf_path:to_html_id(wf_path:get_path()),
			wf:wire(HtmlID, HtmlID, Base#elementbase.actions),
		 	Html = lists:flatten([Module:render(HtmlID, Term)]),
			
			% Restore the old path...
			wf_path:pop_path(),
			Html;
		{_, _} -> []
	end,
	Response.
	
%%% RENDER ACTIONS %%%

render_actions(_, _, undefined) -> [];
render_actions(TriggerPath, TargetPath, Terms=[H|_]) when is_list(Terms), is_integer(H) -> render_actions(TriggerPath, TargetPath, #script { script=Terms });
render_actions(TriggerPath, TargetPath, Terms) when is_list(Terms) -> [render_actions(TriggerPath, TargetPath, X) || X <- Terms];
render_actions(TriggerPath, TargetPath, Term) when is_tuple(Term) ->
	Base = wf_utils:get_actionbase(Term),
	Module = Base#actionbase.module, 

	case Base#actionbase.show_if of 
		true -> 
			% Get the TriggerPaths...	
			TriggerPath1 = case Base#actionbase.trigger of
				undefined -> wf_path:to_path(TriggerPath);
				X1 -> wf_path:to_path(X1)
			end,
	
			% Get the TargetPaths...
			TargetPath1 = case Base#actionbase.target of
				undefined -> wf_path:to_path(TargetPath);
				X2 -> wf_path:to_path(X2)
			end,
	
			% Set the new path...
			OldPath = get(current_path),
			put(current_path, TargetPath1),
			
			% Render the action...
			Script = lists:flatten([Module:render_action(wf_path:to_html_id(TriggerPath1), wf_path:to_html_id(TargetPath1), Term)]),
			
			% Restore the old path...
			put(current_path, OldPath),
			Script;

		false -> 
			[]
	end.
	
%%% AJAX UPDATES %%%
	
update(TargetPath, Terms) -> update(TargetPath, Terms, "wf_update(obj('me'), \"~s\");").
insert_top(TargetPath, Terms) -> update(TargetPath, Terms, "wf_insert_top(obj('me'), \"~s\");").
insert_bottom(TargetPath, Terms) -> update(TargetPath, Terms, "wf_insert_bottom(obj('me'), \"~s\");").

update(TargetPath, Terms, JSFormatString) ->
	UpdateQueue = get(wf_update_queue),
	put(wf_update_queue, [{TargetPath, Terms, JSFormatString}|UpdateQueue]),
	ok.

	
%%% ACTION WIRING %%%

wire(Actions) -> 
	wire(me, me, Actions).

wire(TriggerPath, Actions) ->	
	wire(TriggerPath, TriggerPath, Actions).

wire(TriggerPath, TargetPath, Actions) ->	
	% Add to the queue of wired actions. These will be rendered in get_script().
	ActionQueue = get(wf_action_queue),
	put(wf_action_queue, [{TriggerPath, TargetPath, Actions}|ActionQueue]),
	ok.