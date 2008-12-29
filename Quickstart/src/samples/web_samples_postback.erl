-module (web_samples_postback).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/onecolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "Postbacks".
headline() -> "Postbacks".
right() -> linecount:render().

body() -> [	
	#p{},
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
	#span { text="Mouse Over Me", actions=#event { type=mouseover, postback=span_mousedover } }
].	

event(EventInfo) ->
	wf:wire(#alert { text=wf:f("~p", [EventInfo]) }),
	ok.