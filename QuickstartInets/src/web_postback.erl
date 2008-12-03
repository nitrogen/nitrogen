-module (web_postback).
-include ("wf.inc").
-export ([main/0, event/1]).

main() ->
	Body = #body { title="Postback", body=
		#panel { style="padding: 20px;", body=[
			#h1 { text="Postback Examples" },
			
			#button { text="Press Me", postback=button_pressed },
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
			
			#span { text="Mouse Over Me", actions=#event { type=mouseover, postback=span_mousedover } },
			#p{}
		]}
	},
	wf:render(Body).
	
event(EventInfo) ->
	wf:wire(#alert { text=wf:f("~p", [EventInfo]) }),
	ok.