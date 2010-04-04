-module (index).
-include ("wf.inc").
-compile(export_all).
-include("grid.hrl").

main() -> #template { file="./templates/front.html" }.

title() -> "Nitrogen Web Framework for Erlang".

layout() ->
    #container_12 { body=[
        #grid_12 { body=header() },
        #grid_clear {},

        #grid_6 { alpha=true, body=top_left() },
        #grid_6 { omega=true, body=top_right() },
        #grid_clear {},

        #grid_3 { alpha=true, body=downloads() },
        #grid_3 { body=downloads() },
        #grid_3 { body=downloads() },
        #grid_3 { omega=true, body=downloads() },
        #grid_clear {},

        #grid_12 { body=footer() }
    ]}.

header() ->
    wf:wire(".menu .home", #add_class { class=selected }),
    [
        "
        <a class='downloads' href='/downloads'>DOWNLOADS</a>
        <a class='demos' href='/demos'>DEMOS</a>
        <a class='learn_more' href='/learn_more'>LEARN MORE</a>
        <a class='get_involved' href='/get_involved'>GET INVOLVED</a>
        <a class='donate' href='/donate'>DONATE</a>
        "
    ].

top_left() ->
    [
        #image { image="./images/GreenHead.png" },

        #p { class="section_title", body="DOWNLOAD 2.01" },
        #p { class="section", body=[
            #link { url="/downloads", body=#image {image="/images/downloads/mac_logo.png" }},
            #link { url="/downloads", body=#image { image="/images/downloads/linux_logo_gray.png" }},
	    #link { url="/downloads", body=#image { image="/images/downloads/windows_logo_gray.png" }},
	    #link { url="/downloads", body=#image { image="/images/downloads/erlang_logo.png" }}
        ]}
    ].

top_right() ->
    [
        #p { class="summary", body=[
            "
            <b>Nitrogen Web Framework</b> is the fastest way to
            develop interactive web applications in full-stack Erlang.
            "
        ]},

        #p { class="section_title", body="LATEST NEWS" },
        #p { class="section", body=[
            "
            <a href=/learn_more>Nitrogen Version 2.x</a> adds more flexible JQuery
            integration, improved OTP compliance, and a handler
            interface allowing you to customize things like session
            management and security, and many more features.
            "
        ]}
    ].

footer() ->
    "
    <p class='credits'>
    Copyright &copy; 2008-2010 <a href='http://rklophaus.com'>Rusty Klophaus</a>. 
    <img src='/images/MiniSpaceman.png' style='vertical-align: middle;' />
    Released under the MIT License.
    </p>
    ".

downloads() ->
    [
        #p { class="section_title", body="LATEST NEWS" },
        #p { class="section", body=[
            "
            <a href=/learn_more>Nitrogen Version 2.x</a> adds more flexible JQuery
            integration, improved OTP compliance, and a handler
            interface allowing you to customize things like session
            management and security, and many more features.
            "
        ]}
    ].
