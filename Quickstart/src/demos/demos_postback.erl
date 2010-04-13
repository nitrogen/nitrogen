-module (demos_postback).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "Postbacks".

headline() -> "Postbacks".

left() -> 
    [
        "
        <p>
        Postbacks allow you to build interactive web applications. You
        can wire a Nitrogen element to listen to any Javascript
        event. When the event fires, the element will post back to the
        <code>event/1</code> function on the calling page (or
        alternatively some delegate module) with the element's tag.
        This allows you to use the power of Erlang pattern matching to
        build an event-driven application.
        ",
        linecount:render()
    ].

right() -> 
    [	
	#button { id=test, text="Press Me", postback=button_pressed },

	#p{},
	#link { text="Click Me", postback=link_clicked },

	#p{},	
	#label { text="Press enter in the textbox." },
	#textbox { text="This is a message...", postback=textbox_enterkey },

	#p{},
	#checkbox { text="Toggle Me", postback=checkbox_clicked },

	#p{},	
	#dropdown { postback=dropdown_changed, options=[
            #option { text="Option 1" },
            #option { text="Option 2" },
            #option { text="Option 3" }
	]},

	#p{},	
	#span { text="Mouse Over Me", actions=#event { type=mouseover, postback=span_mousedover } }
    ].	

event(EventInfo) ->
    wf:wire(#alert { text=wf:f("~p", [EventInfo]) }),
    ok.
