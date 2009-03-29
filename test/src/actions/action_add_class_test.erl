-module (action_add_class_test).
-compile([export_all]).

-author("michael@mullistechnologies.com").

-include_lib("eunit/include/eunit.hrl").

-include("wf.inc").

new_add_class_1() ->
  Record = #add_class{class="a_class", speed="a_speed"},
  TriggerPath = "trigger_path",
  TargetPath = "target_path",
  lists:flatten(action_add_class:render_action(TriggerPath, TargetPath, Record)).

basic_test_() ->
  [?_assertEqual("Nitrogen.$current_id='';Nitrogen.$current_path='';jQuery(obj('target_path')).addClass('a_class', \"a_speed\");",
                 new_add_class_1())
  ].

