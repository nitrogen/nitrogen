-module(element_image_test).
-compile([export_all]).

-author("michael@mullistechnologies.com").

-include_lib("eunit/include/eunit.hrl").

-include("wf.inc").

new_image_1() ->
  Rec_image = #image{},
  lists:flatten(element_image:render("123123",Rec_image)).

new_image_2() ->
  Rec_image = #image{class="t_image"},
  lists:flatten(element_image:render("123125",Rec_image)).

new_image_3() ->
  Rec_image = #image{class="t_image", style="color: cyan;", image="http://an_image.com/sample/image.jpg"},
  lists:flatten(element_image:render("123125",Rec_image)).

basic_test_() ->
    [?_assertEqual("<img id='123123' class='image'/>",new_image_1()),
     ?_assertEqual("<img id='123125' class='image t_image'/>",new_image_2()),
     ?_assertEqual("<img id='123125' class='image t_image' style='color: cyan;' src='http://an_image.com/sample/image.jpg'/>",new_image_3())
    ].
