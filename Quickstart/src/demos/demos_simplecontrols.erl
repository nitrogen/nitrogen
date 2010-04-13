-module (demos_simplecontrols).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos433.html" }.

title() -> "Simple Controls".

headline() -> "Simple Controls".

left() ->
    [
        "
        <p>
        Nitrogen lets you build HTML using Erlang record syntax. For
        example, a Level-1 Header is specified as:

        <p>
        <pre>#h1 { text=\"My Header\" }</pre>
        
        <p>
        This page lists some of the simple HTML elements included in
        Nitrogen.

        ", 
        linecount:render() 
    ].

middle() -> 
    [
	#h1 { text="Header 1"},
	#h2 { text="Header 2"},
	#h3 { text="Header 3"},
	#p{},
	#hr{},

	#h2 { text="Forms" },
	#p{},
	#label { text="TextBox" },
	#textbox { text="This is a textbox." }, 

	#p{},
	#label { text="DatePicker Textbox" },
	#datepicker_textbox {  }, 

	#p{},	
	#label { text="TextArea" },
	#textarea { text="This is a textarea." }, 

	#p{},	
	#label { text="Password Box" },
	#password { text="Password" },

        #p{},
        #dropdown { options=[
            #option { text="Dropdown" },
            #option { text="Option 1" },
            #option { text="Option 2" },
            #option { text="Option 3" }
	]},

        #p{},
	#radiogroup { body=[
            #radio { id=myRadio1, text="Option 1", value="1", checked=true }, #br{},
            #radio { id=myRadio2, text="Option 2", value="2" }, #br{},
            #radio { id=myRadio3, text="Option 3", value="3" }, #br{},
            #radio { id=myRadio4, text="Option 4", value="4" }
	]},

        #p{},
	#checkbox { text="Checkbox", checked=true },

        #p{},
	#button { text="Button" }
    ].

right() -> 
    [
        #p{},
	#h2 { text="Lists" },
	#list { body=[
            #listitem { text="List Item 1" },
            #listitem { text="List Item 2" },
            #listitem { body=#checkbox { text="List Item 3" }}
	]},

	#list { numbered=true, body=[
            #listitem { text="List Item 1" },
            #listitem { text="List Item 2" },
            #listitem { body=#checkbox { text="List Item 3" }}
	]},

        #h2 { text="Gravatars"},
        #gravatar{ email="RKlophaus@Gmail.com", size="100", rating="x" }, 
        #p{},
        #gravatar{ email="dan.bravender@test.com", size="100", default="wavatar" }
    ].

event(_) -> ok.
