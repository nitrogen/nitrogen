% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_jquery_effect).
-include ("wf.inc").
-compile(export_all).

render_action(_TriggerPath, TargetPath, Record) ->
	Effect = Record#jquery_effect.effect,
	Speed = Record#jquery_effect.speed, 
	Options = to_js(Record#jquery_effect.options),
	Class = Record#jquery_effect.class,
	Easing = Record#jquery_effect.easing,
	
	Script = case Record#jquery_effect.type of
		'show' when Effect==none -> wf:f("show();");
		'hide' when Effect==none -> wf:f("hide();");
		'appear' -> wf:f("fadeIn(~p);", [Speed]);
		'fade'   -> wf:f("fadeOut(~p);", [Speed]);
		'show'   -> wf:f("show('~s', ~s, ~p);",   [Effect, Options, Speed]);
		'hide'   -> wf:f("hide('~s', ~s, ~p);",   [Effect, Options, Speed]);
		'effect' -> wf:f("effect('~s', ~s, ~p);", [Effect, Options, Speed]);
		'toggle' -> wf:f("toggle('~s', ~s, ~p);", [Effect, Options, Speed]);
		'add_class'    -> wf:f("addClass('~s', ~p);", [Class, Speed]);
		'remove_class' -> wf:f("removeClass('~s', ~p);", [Class, Speed]);
		'animate' -> wf:f("animate(~s, ~p, '~s');", [Options, Speed, Easing])
	end,

	JSID = wf:to_js_id(TargetPath),
	[wf:me_var(), wf:f("$(obj('~s')).~s", [JSID, Script])].
	
to_js(Options) ->
	F = fun({Key, Value}) ->
		if 
			is_list(Value) -> 
				wf:f("~s: '~s'", [Key, wf_utils:js_escape(Value)]);
			is_atom(Value) andalso (Value == true orelse Value == false) ->
				wf:f("~s: ~s", [Key, Value]);
			is_atom(Value) ->
				wf:f("~s: '~s'", [Key, Value]);
			true -> 
				wf:f("~s: ~p", [Key, Value])
		end
	end,
	Options1 = [F(X) || X <- Options],
	Options2 = string:join(Options1, ","),
	wf:f("{ ~s }", [Options2]).
	
				
	
	
