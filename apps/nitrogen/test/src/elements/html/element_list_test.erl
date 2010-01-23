-module(element_list_test).
-compile([export_all]).

-author("michael@mullistechnologies.com").

-include_lib("eunit/include/eunit.hrl").

-include("wf.inc").

new_list_1() ->
  Rec_list = #list{},
  lists:flatten(element_list:render("1",Rec_list)).

new_list_2() ->
  Rec_list = #list{class="t_list"},
  lists:flatten(element_list:render("2",Rec_list)).

new_list_3() ->
  Rec_list = #list{class="t_list", style="color: cyan;", body="SOME BODY"},
  lists:flatten(element_list:render("3",Rec_list)).

new_list_4() ->
  Rec_list = #list{numbered=true},
  lists:flatten(element_list:render("4",Rec_list)).

new_list_5() ->
  Rec_list = #list{numbered=true, class="t_list"},
  lists:flatten(element_list:render("5",Rec_list)).

new_list_6() ->
  Rec_list = #list{class="t_list", style="color: cyan;", numbered=true, body="SOME BODY"},
  lists:flatten(element_list:render("6",Rec_list)).

basic_test_() ->
  [
   ?_assertEqual("<ul id=\"1\" class=\"list\"/>",new_list_1()),
   ?_assertEqual("<ul id=\"2\" class=\"list t_list\"/>",new_list_2()),
   ?_assertEqual("<ul id=\"3\" class=\"list t_list\" style=\"color: cyan;\">SOME BODY</ul>",new_list_3()),
   ?_assertEqual("<ol id=\"4\" class=\"list\"/>",new_list_4()),
   ?_assertEqual("<ol id=\"5\" class=\"list t_list\"/>",new_list_5()),
   ?_assertEqual("<ol id=\"6\" class=\"list t_list\" style=\"color: cyan;\">SOME BODY</ol>",new_list_6()),
   ?_assertEqual([module,id,actions,show_if,class,style,numbered,body],
	 element_list:reflect())
  ].
