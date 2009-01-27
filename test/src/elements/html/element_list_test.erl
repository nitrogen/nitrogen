-module(element_list_test).
-compile([export_all]).

-author("michael@mullistechnologies.com").

-include_lib("eunit/include/eunit.hrl").

-include("wf.inc").

new_list_1() ->
  Rec_list = #list{},
  lists:flatten(element_list:render("123123",Rec_list)).

new_list_2() ->
  Rec_list = #list{class='t_list'},
  lists:flatten(element_list:render("123125",Rec_list)).

new_list_3() ->
  Rec_list = #list{class='t_list', style='color: cyan;', numbered=true, body="SOME BODY"},
  lists:flatten(element_list:render("123125",Rec_list)).

basic_test_() ->
    [?_assertEqual("<list size='1' id='123123' class='list'/>",new_list_1()),
     ?_assertEqual("<list size='1' id='123125' class='list t_list'/>",new_list_2()),
     ?_assertEqual("<list size='1' id='123125' class='list t_list' style='color: cyan;'/>",new_list_3())
    ].
