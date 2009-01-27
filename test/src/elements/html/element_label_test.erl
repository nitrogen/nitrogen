-module(element_label_test).
-compile([export_all]).

-author("michael@mullistechnologies.com").

-include_lib("eunit/include/eunit.hrl").

-include("wf.inc").

new_label_1() ->
  Rec_label = #label{},
  lists:flatten(element_label:render("123123",Rec_label)).

new_label_2() ->
  Rec_label = #label{class='t_label'},
  lists:flatten(element_label:render("123125",Rec_label)).

new_label_3() ->
  Rec_label = #label{class='t_label', style='color: cyan;', text='http://an_label.com/sample/label.jpg'},
  lists:flatten(element_label:render("123125",Rec_label)).

basic_test_() ->
    [?_assertEqual("<label size='1' id='123123' class='label'/>",new_label_1()),
     ?_assertEqual("<label size='1' id='123125' class='label t_label'/>",new_label_2()),
     ?_assertEqual("<label size='1' id='123125' class='label t_label' style='color: cyan;'/>",new_label_3())
    ].
