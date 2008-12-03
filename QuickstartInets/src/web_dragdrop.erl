-module (web_dragdrop).
-include ("wf.inc").
-export ([main/0, event/1, drop_event/2]).
-define (DRAGGABLE_STYLE, "cursor: default; font-size: 10px; text-align: center; width: 80px; height: 80px; padding: 10px; margin: 10px; float: left; border: solid 1px black; background-color: #ccc;").
-define (DROPPABLE_STYLE, "width: 300px; padding: 10px; float: left; border: solid 1px black; height: 150px; margin: 10px; background-color: #abc;").

main() ->
	Body = #body { title="Drag and Drop Example", body=
		#panel { style="padding: 20px;", body=[
			#h1 { text="Drag and Drop Example" },
			#draggable { style=?DRAGGABLE_STYLE, group=group1, tag=drag1, clone=true, body="Drag 1<br>Clone<br>Revert"	},
			#draggable { style=?DRAGGABLE_STYLE, group=group1, tag=drag2, clone=true, revert=false, body="Drag 2<br>Clone<br>No Revert"	},
			#draggable { style=?DRAGGABLE_STYLE, group=group1, tag=drag3, clone=false, revert=true, body="Drag 3<br>No Clone<br>Revert"	},
			#draggable { style=?DRAGGABLE_STYLE, group=group1, tag=drag4, clone=false, revert=false, body="Drag 4<br>No Clone<br>No Revert"	},
			#draggable { style=?DRAGGABLE_STYLE, group=group2, tag=drag5, handle=handle, body=[
				#span { class=handle, text="(Handle)" },
				"<BR>",
				"Drag 5"
			]},
			#p{},
			#droppable { style=?DROPPABLE_STYLE, accept_groups=group1, tag=drop1, body="Drop 1 (accepts Drag 1 to 4)" },
			#droppable { style=?DROPPABLE_STYLE, accept_groups=[group1, group2], tag=drop2, body="Drop 2 (accepts everything)" },
			#p{},
			#flash {}
		]}
	},
	
	
	wf:render(Body).
	
drop_event(DragTag, DropTag) ->
	Message = wf:f("Dropped ~p on ~p", [DragTag, DropTag]),
	wf:flash(Message),
	ok.

event(_) -> ok.