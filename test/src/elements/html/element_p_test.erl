-module(element_p_test).
-compile([export_all]).

-author("michael@mulpechnologies.com").

-include_lib("eunit/include/eunit.hrl").

-include("wf.inc").

new_p_1() ->
  Rec_p = #p{},
  lists:flatten(element_p:render("1",Rec_p)).

new_p_2() ->
  Rec_p = #p{class="t_p"},
  lists:flatten(element_p:render("2",Rec_p)).

new_p_3() ->
  Rec_p = #p{class="t_p", style="color: cyan;"},
  lists:flatten(element_p:render("3",Rec_p)).

basic_test_() ->
  [?_assertEqual("<p id=\"1\" class=\"p\"/>",new_p_1()),
   ?_assertEqual("<p id=\"2\" class=\"p t_p\"/>",new_p_2()),
   ?_assertEqual("<p id=\"3\" class=\"p t_p\" style=\"color: cyan;\"/>",new_p_3()),
   ?_assertEqual([module,id,actions,show_if,class,style,body],
	 element_p:reflect())
  ].
