-module (web_samples_effects).
-include ("wf.inc").
-compile(export_all).

main() ->	#template { file="./wwwroot/twocolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "Effects".
headline() -> "Effects".
right() -> linecount:render().

column1() ->
	Event = #event { target=theDiv, type=click },
	Column1 = [
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
		}}
	],
	wf:render(Column1).
	
column2() -> 
	#panel { class=effects_target, id=theDiv, body=[
		"
			Use the buttons on the left to test different effects on this div.
		"
	]}.
	
event(_) -> ok.