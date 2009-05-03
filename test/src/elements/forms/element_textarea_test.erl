-module(element_textarea_test).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-include("wf.inc").

new_textarea_1() ->
  Rec_textarea = #textarea{},
  lists:flatten(element_textarea:render("1",Rec_textarea)).

new_textarea_2() ->
  Rec_textarea = #textarea{class="t_textarea"},
  lists:flatten(element_textarea:render("2",Rec_textarea)).

new_textarea_3() ->
  Rec_textarea = #textarea{class="t_textarea", style="color: cyan;", text="TEXT"},
  lists:flatten(element_textarea:render("3",Rec_textarea)).

basic_test_() ->
  [?_assertEqual("<textarea id=\"1\" name=\"1\" class=\"textarea\"></textarea>",new_textarea_1()),
   ?_assertEqual("<textarea id=\"2\" name=\"2\" class=\"textarea t_textarea\"></textarea>",new_textarea_2()),
   ?_assertEqual("<textarea id=\"3\" name=\"3\" class=\"textarea t_textarea\" style=\"color: cyan;\">TEXT</textarea>",new_textarea_3()),
   ?_assertEqual([module,id,actions,show_if,class,style,text,html_encode],
	 element_textarea:reflect())
  ].
