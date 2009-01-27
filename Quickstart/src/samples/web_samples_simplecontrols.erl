-module (web_samples_simplecontrols).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/onecolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "Simple Controls".
headline() -> "Simple Controls".
right() -> linecount:render().

body() -> [
	#h1 { text="Header 1"},
	#h2 { text="Header 2"},
	#h3 { text="Header 3"},
	#h4 { text="Header 4"},
	#p{},
	#hr{},
	
	#h3 { text="Forms" },
	#p{},
	#label { text="Label" },
	#value { text="Value" },

	#p{},
	#label { text="TextBox" },
	#textbox { text="This is a textbox." }, 

	#p{},	
	#label { text="TextArea" },
	#textarea { text="This is a textarea." }, 

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
	#button { text="Button" },

	#h4 { text="Unordered List" },
	#p{},
	#list { body=[
		#listitem { text="List Item 1" },
		#listitem { text="List Item 2" },
		#listitem { body=#checkbox { text="List Item 3" }}
	]},

	#h4 { text="Ordered List" },
	#p{},
	#list { numbered=true, body=[
		#listitem { text="List Item 1" },
		#listitem { text="List Item 2" },
		#listitem { body=#checkbox { text="List Item 3" }}
	]},
    #h4 { text="Gravatar Image"},
    #gravatar{ email="RKlophaus@Gmail.com", size="100", rating="x" },
    #p{},
    #gravatar{ email="dan.bravender@test.com", size="100", default="wavatar" }
].
	
event(_) -> ok.