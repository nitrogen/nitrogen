%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(mobile).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).
-author("Jesse Gumm (gumm@sigma-star.com)").

main() -> #template{file="./site/templates/mobile.html"}.

title() -> "Nitrogen Web Framework - Mobile Sample".

body() ->
    [
        "If you can see this, then your Nitrogen installation is working.",
        #p{},
        "Go ahead and enable the sample menu below to test postbacks and links",
        #p{},
        #mobile_toggle{
            on_text="Menu Visible",
            off_text="Menu Hidden",
            selected="off",
            postback=toggle_menu,
            id=menu_on,
            width=200
        },
        #p{},
        #mobile_list{
            id=menu,
            theme=a,
            inset=true,
            style="display:none",
            body=[
                #mobile_list_divider{class=c, text="Sample Mobile Menu"},
                mobile_list_link("Non-mobile Sample Page","/"),
                mobile_list_link("Nitrogen Home","http://nitrogenproject.com"),
                mobile_list_link("jQuery Mobile Home","http://jquerymobile.com"),
                mobile_list_link("Erlang Home","http://erlang.org")
            ]
        }
    ].

mobile_list_link(Text,URL) ->
    #mobile_listitem{
        theme=c,
        body=#link{text=Text, url=URL, mobile_target=false}
    }.

event(toggle_menu) ->
    ShowMenu = wf:q(menu_on),
    case ShowMenu of
        "on" -> wf:wire(menu,#appear{});
        "off" -> wf:wire(menu,#fade{})
    end.
