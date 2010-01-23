-module(element_value_test).
-compile([export_all]).

-author("michael@mulpechnologies.com").

-include_lib("eunit/include/eunit.hrl").

-include("wf.inc").

new_value_1() ->
  Rec_value = #value{},
  lists:flatten(element_value:render("1",Rec_value)).

new_value_2() ->
  Rec_value = #value{class="t_value"},
  lists:flatten(element_value:render("2",Rec_value)).

new_value_3() ->
  Rec_value = #value{class="t_value", style="color: cyan;", text="TEXT"},
  lists:flatten(element_value:render("3",Rec_value)).

basic_test_() ->
  [?_assertEqual("<span id=\"1\" class=\"value\"></span>",new_value_1()),
   ?_assertEqual("<span id=\"2\" class=\"value t_value\"></span>",new_value_2()),
   ?_assertEqual("<span id=\"3\" class=\"value t_value\" style=\"color: cyan;\">TEXT</span>",new_value_3()),
   ?_assertEqual([module,id,actions,show_if,class,style,text,html_encode],
	 element_value:reflect())
  ].
