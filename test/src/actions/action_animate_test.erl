-module (action_animate_test).
-compile([export_all]).

-author("michael@mullistechnologies.com").

-include_lib("eunit/include/eunit.hrl").

-include("wf.inc").

new_animate_1() ->
  Record = #animate{options=[{"key","value"}], speed="a_speed", easing="some_easing"},
  TriggerPath="_trigger_path",
  TargetPath="_target_path",
  lists:flatten(action_animate:render_action(TriggerPath,TargetPath, Record)).

basic_test_() ->
  [?_assertEqual("Nitrogen.$current_id='';Nitrogen.$current_path='';jQuery(obj('_target_path')).animate({ key: 'value' }, \"a_speed\", 'some_easing');", 
                 new_animate_1())
  ].


