-module ([[[NAME]]]).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Hello from [[[NAME]]].erl!".

body() -> 
    [
        #panel { style="margin: 50px 100px;", body=[
            #span { text="Hello from [[[NAME]]].erl!" },
            #p{},
            #button { text="Click me!", postback=click }
        ]}
    ].
	
event(click) ->
    wf:wire(#alert { text="You clicked the button!" }).
