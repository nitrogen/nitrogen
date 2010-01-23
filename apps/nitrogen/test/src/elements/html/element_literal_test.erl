-module(element_literal_test).
-compile([export_all]).

-author("michael@mullistechnologies.com").

-include_lib("eunit/include/eunit.hrl").

-include("wf.inc").

new_literal_1() ->
  Rec_literal = #literal{},
  (element_literal:render("1",Rec_literal)).  % the value does nothing

new_literal_2() ->
  Rec_literal = #literal{class="t_literal"},
  (element_literal:render("2",Rec_literal)).

new_literal_3() ->
  Rec_literal = #literal{class="t_literal", style="color: cyan;", text="http://an_literal.com/sample/literal.jpg"},
  (element_literal:render("3",Rec_literal)).

basic_test_() ->
  [?_assertEqual("",new_literal_1()),
   ?_assertEqual("",new_literal_2()),
   ?_assertEqual("http://an_literal.com/sample/literal.jpg",new_literal_3()),
   ?_assertEqual([module,id,actions,show_if,class,style,text,html_encode],
	 element_literal:reflect())
  ].
