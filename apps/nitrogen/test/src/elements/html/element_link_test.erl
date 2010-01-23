-module(element_link_test).
-compile([export_all]).

-author("michael@mullistechnologies.com").

-include_lib("eunit/include/eunit.hrl").

-include("wf.inc").

new_link_1() ->
  Rec_link = #link{},
  lists:flatten(element_link:render("1",Rec_link)).

new_link_2() ->
  Rec_link = #link{postback="undefined"},
  lists:flatten(element_link:render("2",Rec_link)).

new_link_3() ->
  Rec_link = #link{postback="Postback"},
  lists:flatten(element_link:render("3",Rec_link)).

new_link_4() ->
  Rec_link = #link{body=#image { image="/path/to/image.gif" }},
  lists:flatten(element_link:render("4",Rec_link)).

new_link_5() ->
  Rec_link = #link{body="A LINK BODY", text="LINK TEXT", html_encode=true, url="not_javascript", postback="mypostback"},
  lists:flatten(element_link:render("5",Rec_link)).

basic_test_() ->
  [?_assertEqual("<a id=\"1\" href=\"javascript:\" class=\"link\"/>", new_link_1()),
   ?_assertEqual("<a id=\"2\" href=\"javascript:\" class=\"link\"/>", new_link_2()),
   ?_assertEqual("<a id=\"3\" href=\"javascript:\" class=\"link\"/>", new_link_3()),
   ?_assert(eunit_helper:regexpMatch("<a id=\"4\" href=\"javascript:\" class=\"link\"><img id=\".*?\" class=\"image\" src=\"/path/to/image.gif\"/></a>",
                                     new_link_4())),
   ?_assertEqual("<a id=\"5\" href=\"not_javascript\" class=\"link\">LINK&nbsp;TEXTA LINK BODY</a>", new_link_5()),
   ?_assertEqual([module,id,actions,show_if,class,style,text,body, html_encode,url,postback],
	 element_link:reflect())
  ].
