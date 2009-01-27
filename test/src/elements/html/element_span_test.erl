-module(element_span_test).
-compile([export_all]).

-author("michael@mulpechnologies.com").

-include_lib("eunit/include/eunit.hrl").

-include("wf.inc").

new_span_1() ->
  Rec_span = #span{},
  lists:flatten(element_span:render("123123",Rec_span)).

new_span_2() ->
  Rec_span = #span{class="t_span"},
  lists:flatten(element_span:render("123125",Rec_span)).

new_span_3() ->
  Rec_span = #span{class="t_span", style="color: cyan;", text="Some Text"},
  lists:flatten(element_span:render("123125",Rec_span)).

basic_test_() ->
    [?_assertEqual("<span id='123123'></span>",new_span_1()),
     ?_assertEqual("<span id='123125' class='t_span'></span>",new_span_2()),
     ?_assertEqual("<span id='123125' class='t_span' style='color: cyan;'>Some&nbsp;Text</span>", new_span_3())
    ].
