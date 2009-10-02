-module (action_jquery_effect_test).
-compile([export_all]).

-author("michael@mullistechnologies.com").

-include_lib("eunit/include/eunit.hrl").

-include("wf.inc").

new_jquery_effect_1(EffectName) ->
  % use of quote and double quote on key/value pairs is deliberate
  Record = #jquery_effect{ type=EffectName, effect=none, speed="a_speed", options=[{"Key1","Value1"},{'Key2','Value2'}], class="a_class", easing="some_easing"},
  TriggerPath="_trigger_path",
  TargetPath="_target_path",
  lists:flatten(action_jquery_effect:render_action(TriggerPath,TargetPath, Record)).

new_jquery_effect_2(EffectName) ->
  % The effect  in a jquery_effect is a jquery_ui effect name
  Record = #jquery_effect{ type=EffectName, effect="Pulsate", speed="a_speed", options=[{"Key1","Value1"},{'Key2','Value2'}], class="a_class", easing="some_easing"},
  TriggerPath="_trigger_path",
  TargetPath="_target_path",
  lists:flatten(action_jquery_effect:render_action(TriggerPath,TargetPath, Record)).

basic_test_() ->
  [
  ?_assertEqual("Nitrogen.$current_id='';Nitrogen.$current_path='';jQuery(obj('_target_path')).show();",
                new_jquery_effect_1('show')),
  ?_assertEqual("Nitrogen.$current_id='';Nitrogen.$current_path='';jQuery(obj('_target_path')).show('Pulsate', { Key1: 'Value1',Key2: 'Value2' }, \"a_speed\");",
                new_jquery_effect_2('show')),  % With an Effect
  ?_assertEqual("Nitrogen.$current_id='';Nitrogen.$current_path='';jQuery(obj('_target_path')).hide();",
                new_jquery_effect_1('hide')),
  ?_assertEqual("Nitrogen.$current_id='';Nitrogen.$current_path='';jQuery(obj('_target_path')).hide('Pulsate', { Key1: 'Value1',Key2: 'Value2' }, \"a_speed\");",
                new_jquery_effect_2('hide')),  % With an Effect
  ?_assertEqual("Nitrogen.$current_id='';Nitrogen.$current_path='';jQuery(obj('_target_path')).fadeIn(\"a_speed\");",
                new_jquery_effect_1('appear')),
  ?_assertEqual("Nitrogen.$current_id='';Nitrogen.$current_path='';jQuery(obj('_target_path')).fadeOut(\"a_speed\");",
                new_jquery_effect_1('fade')),
  ?_assertEqual("Nitrogen.$current_id='';Nitrogen.$current_path='';jQuery(obj('_target_path')).effect('none', { Key1: 'Value1',Key2: 'Value2' }, \"a_speed\");",
                new_jquery_effect_1('effect')),
  ?_assertEqual("Nitrogen.$current_id='';Nitrogen.$current_path='';jQuery(obj('_target_path')).toggle();",
                new_jquery_effect_1('toggle')),
  ?_assertEqual("Nitrogen.$current_id='';Nitrogen.$current_path='';jQuery(obj('_target_path')).addClass('a_class', \"a_speed\");",
                new_jquery_effect_1('add_class')),
  ?_assertEqual("Nitrogen.$current_id='';Nitrogen.$current_path='';jQuery(obj('_target_path')).removeClass('a_class', \"a_speed\");",
                new_jquery_effect_1('remove_class')),
  ?_assertEqual("Nitrogen.$current_id='';Nitrogen.$current_path='';jQuery(obj('_target_path')).animate({ Key1: 'Value1',Key2: 'Value2' }, \"a_speed\", 'some_easing');", new_jquery_effect_1('animate'))
  ].

to_js_test_() ->
  [
   ?_assertEqual("{ alist: 'backgroundblue' }", action_jquery_effect:options_to_js([{alist,["background","blue"]}])),
   ?_assertEqual("{ akey: 'WHAT IS THIS SUPPOSED BE?' }", action_jquery_effect:options_to_js([{akey,"WHAT IS THIS SUPPOSED BE?"}])),
   ?_assertEqual("{ akey: true }", action_jquery_effect:options_to_js([{akey,true}])),
   ?_assertEqual("{ akey: false }", action_jquery_effect:options_to_js([{akey,false}])),
   ?_assertEqual("{ akey: 'anatom' }", action_jquery_effect:options_to_js([{akey,'anatom'}])),
   ?_assertEqual("{ akey: 'something else' }", action_jquery_effect:options_to_js([{akey,["something else"]}])),
   ?_assertEqual("{ akey: 152 }", action_jquery_effect:options_to_js([{akey,152}]))
  ].
