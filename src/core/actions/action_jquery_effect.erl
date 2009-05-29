% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_jquery_effect).
-include ("wf.inc").
-compile(export_all).

render_action(Record, Context) ->
	Effect = Record#jquery_effect.effect,
	Speed = Record#jquery_effect.speed, 
	Options = options_to_js(Record#jquery_effect.options),
	Class = Record#jquery_effect.class,
	Easing = Record#jquery_effect.easing,
	
	Script = case Record#jquery_effect.type of
		'show' when Effect==none -> wff:f("show();");
		'hide' when Effect==none -> wff:f("hide();");
		'toggle' when Effect==none -> wff:f("toggle();");
		'appear' -> wff:f("fadeIn(~p);", [Speed]);
		'fade'   -> wff:f("fadeOut(~p);", [Speed]);
		'show'   -> wff:f("show('~s', ~s, ~p);",   [Effect, Options, Speed]);
		'hide'   -> wff:f("hide('~s', ~s, ~p);",   [Effect, Options, Speed]);
		'effect' -> wff:f("effect('~s', ~s, ~p);", [Effect, Options, Speed]);
		'toggle' -> wff:f("toggle('~s', ~s, ~p);", [Effect, Options, Speed]);
		'add_class'    -> wff:f("addClass('~s', ~p);", [Class, Speed]);
		'remove_class' -> wff:f("removeClass('~s', ~p);", [Class, Speed]);
		'animate' -> wff:f("animate(~s, ~p, '~s');", [Options, Speed, Easing])
	end,
	Script1 = wff:f("jQuery(obj('me')).~s", [Script]),
	{ok, Script1, Context}.

%% Options is a list of {Key,Value} tuples	
options_to_js(Options) ->
	F = fun({Key, Value}) ->
		if 
			is_list(Value) -> 
				wff:f("~s: '~s'", [Key, wf_utils:js_escape(Value)]);
			is_atom(Value) andalso (Value == true orelse Value == false) ->
				wff:f("~s: ~s", [Key, Value]);
			is_atom(Value) ->
				wff:f("~s: '~s'", [Key, Value]);
			true -> 
				wff:f("~s: ~p", [Key, Value])
		end
	end,
	Options1 = [F(X) || X <- Options],
	Options2 = string:join(Options1, ","),
	wff:f("{ ~s }", [Options2]).
	
				
	
	
