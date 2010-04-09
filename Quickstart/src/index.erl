-module (index).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/front.html" }.

title() -> "Nitrogen Web Framework for Erlang".

layout() ->
    #container_12 { body=[
        #grid_12 { class=header, body=common:header(home) },
        #grid_clear {},

        #grid_6 { alpha=true, body=top_left() },
        #grid_6 { omega=true, body=top_right() },
        #grid_clear {},

        #grid_12 { body=common:footer() }
    ]}.

top_left() ->
    [
        #p{},
        #image { class=green_head, image="./images/green_head.png" }
    ].

top_right() ->
    [
        #p { class="summary", body=[
            "
            <b>Nitrogen Web Framework</b> is the fastest way to
            develop interactive web applications in full-stack Erlang.
            "
        ]},

        #p { class="section_title", body="DOWNLOAD 2.01" },
        #p { class="section", body=[
            #link { url="/downloads", body=#image { image="/images/downloads/mac_logo.png" }},
            #link { url="/downloads", body=#image { image="/images/downloads/linux_logo_gray.png" }},
	    #link { url="/downloads", body=#image { image="/images/downloads/windows_logo_gray.png" }},
	    #link { url="/downloads", body=#image { image="/images/downloads/erlang_logo.png" }}
        ]},

        #p { class="section_title", body="LATEST NEWS" },
        #p { class="section", body=[
            "
            <a href=/learn_more>Nitrogen Version 2.x</a> adds more flexible JQuery
            integration, improved Erlang OTP compliance, a handler
            interface allowing you to customize things like session
            management and security, and many more features.
            "
        ]}
    ].
