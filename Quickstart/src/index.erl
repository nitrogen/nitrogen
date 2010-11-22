-module (index).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/grid.html" }.

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

        #p { class="section_title", body="DOWNLOAD NITROGEN 2.x" },
        #p { class="section", body=[
            #link { url="/downloads", body=#image { image="/images/downloads/mac_logo.png" }},
	    #link { url="/downloads", body=#image { image="/images/downloads/windows_logo.png" }},
            #link { url="/downloads", body=#image { image="/images/downloads/linux_logo_gray.png" }},
	    #link { url="/downloads", body=#image { image="/images/downloads/erlang_logo.png" }}
        ]},

        #p { class="section_title", body="LATEST NEWS" },
        #p { class="section", body=[
            "
            Learn how to develop a Nitrogen application<br>
            with the <b><a href='/doc/tutorial.html'>Nitrogen Tutorial &raquo;</a></b>
            <p>
            Nitrogen 2.x adds great new elements, actions, and API
            calls, as well as improved packaging and OTP
            compliance. 
            <p>
            <b><a href='/whatsnew'>What's New in Nitrogen 2.x &raquo;</a></b>
            "
        ]}
    ].
