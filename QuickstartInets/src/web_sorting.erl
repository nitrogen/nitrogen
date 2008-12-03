-module (web_sorting).
-include ("wf.inc").
-export ([main/0, event/1, sort_event/2]).

main() ->
	Body = #body { title="Sorting Example", body=
		#panel { style="padding: 20px;", body=[
			#h1 { text="Sorting Examples" },
			"
			Drag to re-order a list, or drag from one list to another.
			",
			#p{},			
			#panel { style="width: 200px; float: left;", body=[
				#h3 { text="Block 1" },
				#sortblock { id=block1, postback=sort, connect_with=block2, body=[
					#sortitem { tag=tag1, body="Item1" },
					#sortitem { tag=tag2, body="Item2" },
					#sortitem { tag=tag3, class=handle, body="Item3" },
					#sortitem { tag=tag4, class=handle, body="Item4" }
				]}
			]},
						
			#panel { style="width: 200px; float: left;", body=[
				#h3 { text="Block 2" },
				#sortblock { style="width: 200px; float: left;", id=block2, postback=sort, connect_with=block1, body=[
					#sortitem { tag=tag5, body="Item5" },
					#sortitem { tag=tag6, body="Item6" },
					#sortitem { tag=tag7, body="Item7" },
					#sortitem { tag=tag8, body="Item8" }
				]}
			]}

		]}
	},
	
	wf:render(Body).
	
sort_event(_Tag, Items) -> 
	Message = wf:f("Order: ~p", [Items]),
	wf:wire(#alert { text=Message }),
	ok.

event(_) -> ok.