-module (web_simplecontrols).
-include ("wf.inc").
-export ([main/0, event/1]).

main() ->
	Body = #body { title="Simple Controls", body=
		#panel { style="padding: 20px;", body=[
			#h1 { text="Simple Controls Example" },
			#h1 { text="Header 1"},
			#h2 { text="Header 2"},
			#h3 { text="Header 3"},
			#h4 { text="Header 4"},
			#label { text="Label" },
			#value { text="Value" },
			#p {},
			#hr {},
			#link { text="Link (to google.com)", url="http://google.com" }, #p{},
			#span { text="Span With Border", style="border: solid 1px black;" }, #p{},
			#button { text="Button" }, #p{},
			#literal { text="Literal" }, #p{},
			#textbox { text="Textbox" }, #p{},
			#textarea { text="Textarea" }, #p{},
			"Password: " ,#password { text="Password" }, #p{},
			#dropdown { options=[
				#option { text="Dropdown" },
				#option { text="Option 1" },
				#option { text="Option 2" },
				#option { text="Option 3" }
			]},
			#p{},
			#checkbox { text="Checkbox", checked=true }
		]}
	},
	wf:render(Body).
	
event(_) -> ok.