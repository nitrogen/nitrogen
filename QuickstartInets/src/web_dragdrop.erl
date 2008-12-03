-module (web_dragdrop).
-include ("wf.inc").
-export ([main/0, event/1]).
-define (DRAGGABLE_STYLE, "width: 200px; padding: 20px; border: solid 1px black; background-color: #ccc;").
-define (DROPPABLE_STYLE, "width: 500px; padding: 20px; border: solid 1px black; background-color: #abc;").

main() ->
	Body = #body { title="Drag and Drop Example", body=
		#panel { style="padding: 20px;", body=[
			#h1 { text="Drag and Drop Example" },
			
			#panel { style=?DRAGGABLE_STYLE, id=drag1, body=[
				#span { class=handle, text="(Handle)" }, 
				#br {},
				"Drag1"
			]}, 
			#p{},
			
			#panel { style=?DRAGGABLE_STYLE, id=drag2, body="Drag2" }, 
			#p{},
			#p{},
			#panel { style=?DROPPABLE_STYLE, id=drop1, body="Drop 1 (accepts Drag1 and Drag2)" }, #p{},
			#panel { style=?DROPPABLE_STYLE, id=drop2, body="Drop 2 (only accepts Drag2)" }, #p{}
		]}
	},
	
	wf:wire(drag1, drop1, #dragdrop { clone=false, postback={drag, drag1, drop1}, handle=handle }),
	wf:wire(drag2, drop1, #dragdrop { clone=true, postback={drag, drag2, drop1} }),
	wf:wire(drag2, drop2, #dragdrop { clone=true, postback={drag, drag2, drop2} }),
	
	wf:render(Body).
	
event({drag, Draggable, Droppable}) ->
	Message = wf:f("Dropped ~p on ~p", [Draggable, Droppable]),
	wf:wire(#alert { text=Message }),
	ok;

event(_) -> ok.