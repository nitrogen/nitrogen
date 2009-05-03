-module(element_listitem_test).
-compile([export_all]).

-author("michael@mullistitemechnologies.com").

-include_lib("eunit/include/eunit.hrl").

-include("wf.inc").

new_listitem_1() ->
  Rec_listitem = #listitem{},
  lists:flatten(element_listitem:render("1",Rec_listitem)).

new_listitem_2() ->
  Rec_listitem = #listitem{class="t_listitem"},
  lists:flatten(element_listitem:render("2",Rec_listitem)).

new_listitem_3() ->
  Rec_listitem = #listitem{class="t_listitem", style="color: cyan;", text="http://an_listitem.com/sample/listitem.jpg"},
  lists:flatten(element_listitem:render("3",Rec_listitem)).

basic_test_() ->
  [?_assertEqual("<li id=\"1\" class=\"listitem\"/>",new_listitem_1()),
   ?_assertEqual("<li id=\"2\" class=\"listitem t_listitem\"/>",new_listitem_2()),
   ?_assertEqual("<li id=\"3\" class=\"listitem t_listitem\" style=\"color: cyan;\">http://an_listitem.com/sample/listitem.jpg</li>",new_listitem_3()),
   ?_assertEqual([module,id,actions,show_if,class,style,body,text,html_encode],
	 element_listitem:reflect())
  ].
