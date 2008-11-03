-module (wf_render).
-include ("wf.inc").
-export ([
	me_var/0,
	
	render/1,
	render_actions/3,
	
	update/2,
	insert_top/2,
	insert_bottom/2,
	
	unsafe_update/2,
	unsafe_insert_top/2,
	unsafe_insert_bottom/2,
	
	wire/1, wire/2, wire/3
]).

me_var() -> 
	Path = get(current_path),
	["var me=", wf_path:js_id(Path), "; "].

render(undefined) -> "";
render(Term) when is_binary(Term) -> Term;
render(Terms=[H|_]) when is_list(Terms), is_integer(H) -> Terms;
render(Terms) when is_list(Terms) ->[render(X) || X <- Terms];
render(Term) when is_tuple(Term) ->
	% Load vars...
	Type = element(1, Term),
	ID = case wf:to_atom(element(2, Term)) of
		undefined -> wf:temp_id();
		Other -> Other
	end,
	Actions = element(3, Term),
	ShowIf = element(4, Term),
	TypeModule = list_to_atom("element_" ++ atom_to_list(Type)),
	
	Response = case {ShowIf, wf_path:is_temp_element(ID)} of
		{true, true} -> 
			% This is a temp element, so no need to register the path.
			Path = get(current_path),
			ParentJSID = wf_path:js_id(Path),
			JSID = wf_path:js_id(ID),
			HtmlID = wf_path:html_id(ID),
			
			wf_script:add_dom_script(wf:f("wf_link('~s', '~s', '~s');", [ParentJSID, JSID, HtmlID])),
			Html = lists:flatten([TypeModule:render(HtmlID, Term)]),
			wf:wire(HtmlID, HtmlID, Actions),
			Html;

		{true, false} -> 
			% Set the new path...
			OldPath = get(current_path),
			Path = [ID|OldPath],
			put(current_path, Path),
			
			% Register the control path...
			ParentJSID = wf_path:parent_js_id(Path),
			JSID = wf_path:js_id(Path),
			HtmlID = wf_path:html_id(Path),
			wf_path:register_path(HtmlID, Path),

			% Render the control...
			wf_script:add_dom_script(wf:f("wf_link('~s', '~s', '~s', '~s');", [ParentJSID, JSID, HtmlID, wf_utils:pickle({HtmlID, Path})])),
		 	Html = lists:flatten([TypeModule:render(HtmlID, Term)]),
			wf:wire(HtmlID, HtmlID, Actions),
			
			% Restore the old path...
			put(current_path, OldPath),
			Html;
		{_, _} -> []
	end,
	%io:format("~s", [Response]),
	Response.
	
%%% RENDER ACTIONS %%%

render_actions(_, _, undefined) -> [];
render_actions(TriggerPath, TargetPath, Terms=[H|_]) when is_list(Terms), is_integer(H) -> render_actions(TriggerPath, TargetPath, #script { script=Terms });
render_actions(TriggerPath, TargetPath, Terms) when is_list(Terms) -> [render_actions(TriggerPath, TargetPath, X) || X <- Terms];
render_actions(TriggerPath, TargetPath, Term) when is_tuple(Term) ->
	% Vars...
	Type = element(1, Term),
	ShowIf = element(5, Term),
	TypeModule = list_to_atom("action_" ++ atom_to_list(Type)),

	case ShowIf of 
		true -> 
			% Get the TriggerPaths...	
			TriggerPath1 = case element(2, Term) of
				undefined -> wf_path:to_path(TriggerPath);
				X1 -> wf_path:to_path(X1)
			end,
	
			% Get the TargetPaths...
			TargetPath1 = case element(3, Term) of
				undefined -> wf_path:to_path(TargetPath);
				X2 -> wf_path:to_path(X2)
			end,
	
			% Set the new path...
			OldPath = get(current_path),
			put(current_path, TargetPath1),
			
			% Render the action...
			Script = lists:flatten([TypeModule:render_action(wf_path:to_ident(TriggerPath1), wf_path:to_ident(TargetPath1), Term)]),
			
			% Restore the old path...
			put(current_path, OldPath),
			Script;

		false -> 
			[]
	end.
	
unsafe_update(TargetPath, Terms) -> update(TargetPath, Terms, false, "obj(me).update(\"~s\");").
unsafe_insert_top(TargetPath, Terms) -> update(TargetPath, Terms, false, "obj(me).insert({ 'top' : \"~s\");").
unsafe_insert_bottom(TargetPath, Terms) -> update(TargetPath, Terms, false, "obj(me).insert({ 'bottom' : \"~s\");").

update(TargetPath, Terms) -> update(TargetPath, Terms, true, "obj(me).update(\"~s\");").
insert_top(TargetPath, Terms) -> update(TargetPath, Terms, true, "obj(me).insert({ 'top' : \"~s\"});").
insert_bottom(TargetPath, Terms) -> update(TargetPath, Terms, true, "obj(me).insert({ 'bottom' : \"~s\"});").

update(TargetPath, Terms, HtmlEncode, JSFormatString) ->
	Terms1 = case HtmlEncode andalso is_list(Terms) andalso length(Terms) > 0 andalso is_integer(hd(Terms)) of
		true -> wf:html_encode(Terms);
		false -> Terms
	end,
	UpdateQueue = get(wf_update_queue),
	put(wf_update_queue, [{TargetPath, Terms1, JSFormatString}|UpdateQueue]),
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