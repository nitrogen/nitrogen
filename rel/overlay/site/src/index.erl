-module (index).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Welcome to Nitrogen".

body() -> 
    #panel { style="width: 60%; margin: 50px auto;", body=[
        #h1 { text="Welcome to Nitrogen" },
        #p{},
        "
        If you can see this page, then your Nitrogen server is up and
        running. Click the button below to test postbacks.
        ",
        #p{},
        #button { text="Click me!", postback=click },
        #p{},
        "
        Run <b>./bin/dev help</b> to see some useful developer commands.
        "
    ]}.
	
event(click) ->
    wf:wire(#alert { text="You clicked the button!" }).
