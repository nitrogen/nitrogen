-module(element_link_test).
-compile([export_all]).

-author("michael@mullistechnologies.com").

-include_lib("eunit.hrl").

-include("wf.inc").

new_link_1() ->
  Rec_link = #link{},
  lists:flatten(element_link:render("123121",Rec_link)).

new_link_2() ->
  Rec_link = #link{postback='undefined'},
  lists:flatten(element_link:render("123122",Rec_link)).

new_link_3() ->
  Rec_link = #link{postback='Postback'},
  lists:flatten(element_link:render("123123",Rec_link)).

new_link_4() ->
  Rec_link = #link{body="A LINK BODY"},
  lists:flatten(element_link:render("123124",Rec_link)).

new_link_5() ->
  Rec_link = #link{body="A LINK BODY", text="LINK TEXT", html_encode=true, url="not_javascript", postback="mypostback"},
  lists:flatten(element_link:render("123124",Rec_link)).

basic_test_() ->
    [?_assertEqual("<a id='123121' href='javascript:' class='link'></a>", new_link_1()),
     ?_assertEqual("<a id='123122' href='javascript:' class='link'></a>", new_link_2()),
     ?_assertEqual("<a id='123123' href='javascript:' class='link'></a>", new_link_3()),
     ?_assertEqual("<a id='123124' href='javascript:' class='link'>A LINK BODY</a>", new_link_4()),
     ?_assertEqual("<a id='123124' href='not_javascript' class='link'>LINK&nbsp;TEXTA LINK BODY</a>", new_link_5())
    ].
