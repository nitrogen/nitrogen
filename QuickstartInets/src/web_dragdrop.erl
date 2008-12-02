-module (web_dragdrop).
-include ("wf.inc").
-export ([main/0, event/1]).

main() ->
	Body = #body { title="Drag and Drop Example", body=
		#panel { style="padding: 20px;", body=[
			#h1 { text="AJAX Examples" },
			
			#panel { id=drag1, body="Drag 1" }, #p{},
			#panel { id=drag2, body="Drag 2" }, #p{},
			#panel { id=drop1, body="Drop 1" }, #p{},
			#panel { id=drop2, body="Drop 2" }, #p{}
		]}
	},
	
	wf:wire(drag1, drop1, #dragdrop { clone=false, postback={drag, drag1, drop1} }),
	wf:wire(drag2, drop1, #dragdrop { clone=true, postback={drag, drag2, drop1} }),
	
	wf:render(Body).
	
event({drag, Draggable, Droppable}) ->
	Message = wf:f("Dropped ~p on ~p", [Draggable, Droppable]),
	wf:wire(#alert { text=Message }),
	ok;

event(_) -> ok.