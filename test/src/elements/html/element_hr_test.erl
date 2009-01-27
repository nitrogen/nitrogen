-module(element_hr_test).
-compile([export_all]).

-author("michael@mullistechnologies.com").

-include_lib("eunit/include/eunit.hrl").

-include("wf.inc").

new_hr_1() ->
  Rec_hr = #hr{},
  lists:flatten(element_hr:render("123123",Rec_hr)).

new_hr_2() ->
  Rec_hr = #hr{class="t_hr"},
  lists:flatten(element_hr:render("123125",Rec_hr)).

new_hr_3() ->
  Rec_hr = #hr{class="t_hr", style="color: cyan;"},
  lists:flatten(element_hr:render("123125",Rec_hr)).

basic_test_() ->
    [?_assertEqual("<hr size='1' id='123123' class='hr'/>",new_hr_1()),
     ?_assertEqual("<hr size='1' id='123125' class='hr t_hr'/>",new_hr_2()),
     ?_assertEqual("<hr size='1' id='123125' class='hr t_hr' style='color: cyan;'/>",new_hr_3())
    ].
