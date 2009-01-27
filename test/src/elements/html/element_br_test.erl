-module(element_br_test).
-compile([export_all]).

-author("michael@mullistechnologies.com").

-include_lib("eunit.hrl").

-include("wf.inc").

new_br_1() ->
  Rec_br = #br{},
  lists:flatten(element_br:render("123123",Rec_br)).

new_br_2() ->
  Rec_br = #br{class='t_br'},
  lists:flatten(element_br:render("123125",Rec_br)).

new_br_with_style() ->
  Rec_br = #br{class='t_br', style='color: cyan;'},
  lists:flatten(element_br:render("123125",Rec_br)).

basic_test_() ->
    [?_assertEqual("<br id='123123' class='br'/>",new_br_1()),
     ?_assertEqual("<br id='123125' class='br t_br'/>",new_br_2()),
     ?_assertEqual("<br id='123125' class='br t_br' style='color: cyan;'/>",new_br_with_style())
    ].

