-module(element_listitem_test).
-compile([export_all]).

-author("michael@mullistitemechnologies.com").

-include_lib("eunit/include/eunit.hrl").

-include("wf.inc").

new_listitem_1() ->
  Rec_listitem = #listitem{},
  lists:flatten(element_listitem:render("123123",Rec_listitem)).

new_listitem_2() ->
  Rec_listitem = #listitem{class="t_listitem"},
  lists:flatten(element_listitem:render("123125",Rec_listitem)).

new_listitem_3() ->
  Rec_listitem = #listitem{class="t_listitem", style="color: cyan;", text="http://an_listitem.com/sample/listitem.jpg"},
  lists:flatten(element_listitem:render("123125",Rec_listitem)).

basic_test_() ->
    [?_assertEqual("<li id='123123' class='listitem'></li>",new_listitem_1()),
     ?_assertEqual("<li id='123125' class='listitem t_listitem'></li>",new_listitem_2()),
     ?_assertEqual("<li id='123125' class='listitem t_listitem' style='color: cyan;'>http://an_listitem.com/sample/listitem.jpg</li>",new_listitem_3())
    ].
