-module (demos).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/grid.html" }.

title() -> "Demos".

layout() -> 
    #container_12 { body=[
        #grid_12 { alpha=true, omega=true, class=header, body=common:header(demos) },
        #grid_clear {},

        #grid_10 { alpha=true, omega=true, prefix=1, suffix=1, class=headline, body=headline() },
        #grid_clear {},

        #grid_4 { alpha=true, prefix=1, body=left(), class=pad_right },
        #grid_3 { body=middle() },
        #grid_3 { omega=true, suffix=1, body=right() },
        #grid_clear {},

        #grid_12 { alpha=true, omega=true, body=common:footer() }
    ]}.

headline() -> 
    "Demos".

left() -> 
    [
        "
        Click a link on the right to see Nitrogen in action.  
        <p>
        Each demo is a separate Erlang module.  You can view the
        source code of the module using the 'View Module Source' link
        on the left side of the page.

        "
    ].

middle() -> 
    [
	#h2 { text="Controls and Validation" },
	#p{},
	#link { text="Simple Controls", url="/demos/simplecontrols" }, #br{}, 
	#link { text="In-Place Textbox", url="/demos/advancedcontrols1" }, #br{}, 
	#link { text="Google Charts", url="/demos/advancedcontrols2" }, #br{}, 
	#link { text="Radio Buttons", url="/demos/radio" }, #br{}, 
	#link { text="File Uploading", url="/demos/upload" }, #br{}, 
	#link { text="User Notices", url="/demos/notices" }, #br{}, 
	#link { text="Validation", url="/demos/validation" }, #br{},
  #link { text="Autocompletion", url="/demos/textbox_autocomplete" }, #br{}, 

	#h2 { text="Drag, Drop & Sort" },
	#p{},
	#link { text="Drag and Drop", url="/demos/dragdrop" }, #br{}, 
	#link { text="Sorting", url="/demos/sorting1" }, #br{}, 
	#link { text="Nested Sorting", url="/demos/sorting2" }, #br{},

	#h2 { text="Data Binding" },
	#p{},
	#link { text="Simple (List-Based) Binding", url="/demos/binding1" }, #br{}, 
	#link { text="Record-Based Binding", url="/demos/binding2" }, #br{}, 
	#link { text="Key/Value Pair Binding", url="/demos/binding3" }, #br{}, 
	#link { text="Binding With a Transform Function", url="/demos/binding4" }, #br{}
].

right() ->
    [
	#h2 { text="Events and Ajax" },
	#p{},
	#link { text="Effects", url="/demos/effects" }, #br{}, 
	#link { text="Postbacks", url="/demos/postback" }, #br{}, 
	#link { text="AJAX Updates", url="/demos/ajax" }, #br{}, 
	#link { text="AJAX Replace", url="/demos/replace" }, #br{},
	#link { text="AJAX Remove", url="/demos/remove" }, #br{},
	#link { text="JQuery Paths", url="/demos/jquerypaths" }, #br{}, 
	#link { text="Javascript API", url="/demos/api" }, #br{},

	#h2 { text="Comet/Asynchronous Calls" },
	#p{},
	#link { text="Counter with Comet", url="/demos/comet1" }, #br{},
	#link { text="Chatroom with Comet", url="/demos/comet2" }, #br{},
	#link { text="Multiple Comets with Graceful Exit", url="/demos/comet3" }, #br{},
	#link { text="Continuations", url="/demos/continuations" }, #br{},

	#h2 { text="Advanced Topics" },
	#p{},
	#link { text="Set Content Type", url="/demos/contenttype" }, #br{},
	#link { text="HTTP Headers", url="/demos/headers" }, #br{},
    	#link { text="Security", url="/demos/security" }, #br{},
        #link { text="State", url="/demos/state" }
    ].	
