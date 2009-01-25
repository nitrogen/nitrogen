-module (web_samples).
-include ("wf.inc").
-compile(export_all).

main() ->	#template { file="./wwwroot/twocolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.
title() -> "Nitrogen Samples".
headline() -> "Nitrogen Samples".
right() -> linecount:render().

column1() -> [
	#h3 { text="Controls and Validation" },
	#p{},
	#link { text="Simple Controls", url="/web/samples/simplecontrols" }, #br{}, 
	#link { text="In-Place Textbox", url="/web/samples/advancedcontrols1" }, #br{}, 
	#link { text="Google Charts", url="/web/samples/advancedcontrols2" }, #br{}, 
	#link { text="Step-by-Step Wizard", url="/web/samples/advancedcontrols3" }, #br{}, 
	#link { text="Validation", url="/web/samples/validation" }, #br{}, 

	#h3 { text="User Interface" },
	#p{},
	#link { text="Effects", url="/web/samples/effects" }, #br{}, 
	#link { text="Drag and Drop", url="/web/samples/dragdrop" }, #br{}, 
	#link { text="Sorting", url="/web/samples/sorting1" }, #br{}, 
	#link { text="Nested Sorting", url="/web/samples/sorting2" }, #br{}
].

column2() -> [
	#h3 { text="Effects, Events, and Ajax" },
	#p{},
	#link { text="Postbacks", url="/web/samples/postback" }, #br{}, 
	#link { text="AJAX Replacement", url="/web/samples/ajax" }, #br{}, 
	#link { text="User Notices", url="/web/samples/notices" }, #br{}, 

	#h3 { text="Data Binding" },
	#p{},
	#link { text="Simple (List-Based) Binding", url="/web/samples/binding1" }, #br{}, 
	#link { text="Record-Based Binding", url="/web/samples/binding2" }, #br{}, 
	#link { text="Key/Value Pair Binding", url="/web/samples/binding3" }, #br{}, 
	#link { text="Binding With a Transform Function", url="/web/samples/binding4" }, #br{}, 

	#h3 { text="Advanced Topics" },
	#p{},
	#link { text="Counter with Comet", url="/web/samples/comet1" }, #br{},
	#link { text="Chatroom with Comet", url="/web/samples/comet2" }, #br{},
	#link { text="Multiple Comets with Graceful Exit", url="/web/samples/comet3" }, #br{},
	#link { text="Continuations", url="/web/samples/continuations" }, #br{},
	#link { text="Set Content Type", url="/web/samples/contenttype" }	
].
	
event(go) ->
	wf:flash("Hello there"),
	wf:update(test, "This is a test.");
	
event(_) -> ok.