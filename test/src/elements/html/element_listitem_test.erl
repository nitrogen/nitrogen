-module(element_listitem_test).
-compile([export_all]).

-author("michael@mullistitemechnologies.com").

-include_lib("eunit/include/eunit.hrl").

-include("wf.inc").

new_listitem_1() ->
  Rec_listitem = #listitem{},
  listitems:flatten(element_listitem:render("123123",Rec_listitem)).

new_listitem_2() ->
  Rec_listitem = #listitem{class='t_listitem'},
  listitems:flatten(element_listitem:render("123125",Rec_listitem)).

new_listitem_3() ->
  Rec_listitem = #listitem{class='t_listitem', style='color: cyan;', text='http://an_listitem.com/sample/listitem.jpg'},
  listitems:flatten(element_listitem:render("123125",Rec_listitem)).

basic_test_() ->
    [?_assertEqual("<listitem size='1' id='123123' class='listitem'/>",new_listitem_1()),
     ?_assertEqual("<listitem size='1' id='123125' class='listitem t_listitem'/>",new_listitem_2()),
     ?_assertEqual("<listitem size='1' id='123125' class='listitem t_listitem' style='color: cyan;'/>",new_listitem_3())
    ].
