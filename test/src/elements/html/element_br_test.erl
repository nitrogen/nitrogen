-module(element_br_test).
-compile([export_all]).

-author("michael@mullistechnologies.com").

-include_lib("eunit/include/eunit.hrl").

-include("wf.inc").

new_br_1() ->
  Rec_br = #br{},
  lists:flatten(element_br:render("1",Rec_br)).

new_br_2() ->
  Rec_br = #br{class="t_br"},
  lists:flatten(element_br:render("2",Rec_br)).

new_br_with_style() ->
  Rec_br = #br{class="t_br", style="color: cyan;"},
  lists:flatten(element_br:render("3",Rec_br)).

basic_test_() ->
  [?_assertEqual("<br id=\"1\" class=\"br\"/>",new_br_1()),
   ?_assertEqual("<br id=\"2\" class=\"br t_br\"/>",new_br_2()),
   ?_assertEqual("<br id=\"3\" class=\"br t_br\" style=\"color: cyan;\"/>",new_br_with_style()),
   ?_assertEqual([module,id,actions,show_if,class,style],
	 element_br:reflect())
  ].
