-module (demos_effects).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos433.html" }.

title() -> "Effects".

headline() -> "Effects".

left() -> 
    [
        "
        <p>
        Actions such as <code>#toggle{}</code> and <code>#effect{}</code>
        allow you to perform JQuery effects on Nitrogen page element.

        <p> 
        Elements can be referenced either by their Nitrogen ID name,
        or by a JQuery selector.

        ",
        linecount:render()
    ].

middle() ->
    Event = #event { target=theDiv, type=click },
    [
        #link { text="Show", actions=Event#event { 
            actions=#show{} 
        }}, 
        #br{},

        #link { text="Hide", actions=Event#event { 
            actions=#hide{} 
        }}, 
        #br{},

        #link { text="Appear", actions=Event#event { 
            actions=#appear{} 
        }}, 
        #br{},

        #link { text="Fade", actions=Event#event { 
            actions=#fade{} 
        }}, 
        #br{},				

        #link { text="Show - Explode", actions=Event#event { 
            actions=#show{ effect=explode }
        }}, 
        #br{},

        #link { text="Hide - Puff", actions=Event#event { 
            actions=#hide{ effect=puff }
        }}, 
        #br{},

        #link { text="Effect - Bounce", actions=Event#event { 
            actions=#effect{ effect=bounce }
        }}, 
        #br{},

        #link { text="Effect - Highlight", actions=Event#event { 
            actions=#effect{ effect=highlight }
        }}, 
        #br{},

        #link { text="Effect - Shake", actions=Event#event { 
            actions=#effect{ effect=shake }
        }}, 
        #br{},

        #link { text="Toggle - Drop", actions=Event#event { 
            actions=#toggle{ effect=drop, options=[{direction, up}] }
        }}, 
        #br{},

        #link { text="Add Class - BigFont", actions=Event#event {
            actions=#add_class { class=bigfont }
        }},
        #br{},

        #link { text="Remove Class - BigFont", actions=Event#event {
            actions=#remove_class { class=bigfont }
        }},
        #br{},

        #link { text="Animate - Big Border", actions=Event#event { 
            actions=#animate { options=[{borderWidth, "20px"}] }
        }},
        #br{},

        #link { text="Animate - Small Border", actions=Event#event { 
            actions=#animate { options=[{borderWidth, "1px"}] }
        }},
        #br{},

        #link { text="Chaining effects - Shake then Puff", actions=Event#event { 
            actions=#effect { effect=shake, actions=#hide{ effect=puff } }
        }}
    ].

right() -> 
    [
        #panel { class=effects_target, id=theDiv, body=[
            "
            Use the buttons on the left to test different effects on this div.
"
]}
    ].
	
event(_) -> ok.
