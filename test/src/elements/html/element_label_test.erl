-module(element_label_test).
-compile([export_all]).

-author("michael@mullistechnologies.com").

-include_lib("eunit/include/eunit.hrl").

-include("wf.inc").

new_label_1() ->
  Rec_label = #label{},
  lists:flatten(element_label:render("1",Rec_label)).

new_label_2() ->
  Rec_label = #label{class="t_label"},
  lists:flatten(element_label:render("2",Rec_label)).

new_label_3() ->
  Rec_label = #label{class="t_label", style="color: cyan;", text="Username:"},
  lists:flatten(element_label:render("3",Rec_label)).

basic_test_() ->
  [?_assertEqual("<label id=\"1\" class=\"label\"></label>",new_label_1()),
   ?_assertEqual("<label id=\"2\" class=\"label t_label\"></label>",new_label_2()),
   ?_assertEqual("<label id=\"3\" class=\"label t_label\" style=\"color: cyan;\">Username:</label>",new_label_3()),
   ?_assertEqual([module,id,actions,show_if,class,style,text,html_encode],
	 element_label:reflect())
  ].
