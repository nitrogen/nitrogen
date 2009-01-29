-module (action_alert_test).
-compile([export_all]).

-author("michael@mullistechnologies.com").

-include_lib("eunit/include/eunit.hrl").

-include("wf.inc").

new_alert_1() ->
  Record = #alert{text="some_text"},
  TriggerPath="_trigger_path",
  TargetPath="_target_path",
  lists:flatten(action_alert:render_action(TriggerPath,TargetPath, Record)).

basic_test_() ->
  [?_assertEqual("alert(\"some_text\");", new_alert_1())
  ].


