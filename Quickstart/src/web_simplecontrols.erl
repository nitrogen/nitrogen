-module (web_simplecontrols).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./templates/onecolumn.html" }.
title() -> "Simple Controls".
headline() -> "Simple Controls".

body() -> [
	#h1 { text="Header 1"},
	#h2 { text="Header 2"},
	#h3 { text="Header 3"},
	#h4 { text="Header 4"},
	#p{},
	#hr{},
	
	#h3 { text="Forms" },
	#label { text="Label" },
	#value { text="Value" },

	#label { text="TextBox" },
	#textbox { text="" }, 
	#p{},
	
	#label { text="TextArea" },
	#textarea { text="" }, 
	#p{},
	
	#label { text="Password Box" },
	#password { text="Password" }, 
	#p{},
	
	#label { text="DropDown" },
	#dropdown { options=[
		#option { text="Dropdown" },
		#option { text="Option 1" },
		#option { text="Option 2" },
		#option { text="Option 3" }
	]},
	#p{},
	
	#label { text="Checkbox" },
	#checkbox { text="Checkbox", checked=true },
	#p{},
	
	#button { text="Button" }
].
	
event(_) -> ok.