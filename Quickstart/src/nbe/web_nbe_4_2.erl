-module (web_nbe_4_2).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/nbe.html" }.	

css() -> "
	.draggable, .droppable {
		text-align: center; float: left; width: 120px; 
		margin: 10px; padding: 10px; 
		color: #fff; border: solid 1px black; 
	}
	.draggable { background: #369; }
	.droppable { background: #BCD; }
	.droppable.active { border: dashed 1px black;	}
	.droppable.hover { 	border: solid 1px #FFFF00;	}
".

body() -> 
	[
		#draggable { tag="Red", class=draggable, body="Red" },
		#draggable { tag="Blue", class=draggable, body="Blue" },
		#draggable { tag="Green", class=draggable, body="Green" },
		#p { style="clear: both;" },
		#droppable { tag="Sweater", class=droppable, body="Sweater" },
		#droppable { tag="Tie", class=droppable, body="Tie" },
		#droppable { tag="Jacket", class=droppable, body="Jacket" },
		#p { style="clear: both;" },
		#panel { id=mypanel }		
	].

drop_event(DragTag, DropTag) ->
	S = wf:f("You made a ~s ~s!", [DragTag, DropTag]),
	Term = #panel { body=S, actions=#effect { 
		effect=pulsate, speed=300, options=[{times, 1}] }
	},
	wf:insert_top(mypanel, Term).