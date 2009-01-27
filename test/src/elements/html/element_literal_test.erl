-module(element_literal_test).
-compile([export_all]).

-author("michael@mullistechnologies.com").

-include_lib("eunit/include/eunit.hrl").

-include("wf.inc").

new_literal_1() ->
  Rec_literal = #literal{},
  (element_literal:render("123123",Rec_literal)).

new_literal_2() ->
  Rec_literal = #literal{class='t_literal'},
  (element_literal:render("123125",Rec_literal)).

new_literal_3() ->
  Rec_literal = #literal{class='t_literal', style='color: cyan;', text='http://an_literal.com/sample/literal.jpg'},
  (element_literal:render("123125",Rec_literal)).

basic_test_() ->
    [?_assertEqual("<literal id='123123' class='literal'/>",new_literal_1()),
     ?_assertEqual("<literal id='123125' class='literal t_literal'/>",new_literal_2()),
     ?_assertEqual("<literal id='123125' class='literal t_literal' style='color: cyan;'/>",new_literal_3())
    ].
