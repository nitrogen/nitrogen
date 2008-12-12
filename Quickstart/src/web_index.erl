-module (web_index).
-include ("wf.inc").
-export ([main/0, event/1]).

main() ->
	Title = "Nitrogen Examples",
	Body = #template { file=twocolumn, title=Title, headline=Title,
		section1=[
			#h3 { text="Controls and Validation" },
			#link { text="Simple Controls", url="/web/simplecontrols" }, #br{}, 
			#link { text="In-Place Textbox", url="/web/advancedcontrols1" }, #br{}, 
			#link { text="Google Charts", url="/web/advancedcontrols2" }, #br{}, 
			#link { text="Step-by-Step Wizard", url="/web/advancedcontrols3" }, #br{}, 
			#link { text="Validation", url="/web/validation" }, #br{}, 
			#p{},
		
			#h3 { text="User Interface" },
			#link { text="Effects", url="/web/effects" }, #br{}, 
			#link { text="Drag and Drop", url="/web/dragdrop" }, #br{}, 
			#link { text="Sorting", url="/web/sorting1" }, #br{}, 
			#link { text="Nested Sorting", url="/web/sorting2" }, #br{}
		],
		section2=[
			#h3 { text="Effects, Events, and Ajax" },
			#link { text="Postbacks", url="/web/postback" }, #br{}, 
			#link { text="AJAX Replacement", url="/web/ajax" }, #br{}, 
			#link { text="User Notices", url="/web/notices" }, #br{}, 
			#p{},
		
			#h3 { text="Data Binding" },
			#link { text="Simple (List-Based) Binding", url="/web/binding1" }, #br{}, 
			#link { text="Record-Based Binding", url="/web/binding2" }, #br{}, 
			#link { text="Key/Value Pair Binding", url="/web/binding3" }, #br{}, 
			#link { text="Binding With a Transform Function", url="/web/binding4" }, #br{}, 
			#p{},
		
			#h3 { text="Advanced Topics" },
			#link { text="Counter with Comet", url="/web/comet1" }, #br{},
			#link { text="Chatroom with Comet", url="/web/comet2" }, #br{},
			#link { text="Continuations", url="/web/continuations" }
		]
	},
	wf:render(Body).
	
event(go) ->
	wf:flash("Hello there"),
	wf:update(test, "This is a test.");
	
event(_) -> ok.