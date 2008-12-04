-module (web_sorting2).
-include ("wf.inc").
-export ([main/0, event/1, sort_event/2]).

main() ->
	Title = "Nested Sorting",
	Body = #template { file=onecolumn, title=Title, headline=Title, section1=[

		#sortblock { class=advanced, postback=sort, handle=handle, body=[
			#sortitem { tag=parent1, body=#panel { body=[
				#span { class=handle, text="(Grab)" }, " Parent 1",
				#sortblock { postback=child1sort, body=[
					#sortitem { tag=child1_1, body="Child 1" },
					#sortitem { tag=child1_2, body="Child 2" },
					#sortitem { tag=child1_3, body="Child 3" },
					#sortitem { tag=child1_4, body="Child 4" }
				]}
			]}},
			#sortitem { tag=parent2, body=#panel { body=[
				#span { class=handle, text="(Grab)" }, " Parent 2",
				#sortblock { postback=child2sort, body=[
					#sortitem { tag=child2_1, body="Child 1" },
					#sortitem { tag=child2_2, body="Child 2" },
					#sortitem { tag=child2_3, body="Child 3" },
					#sortitem { tag=child2_4, body="Child 4" }
				]}
			]}},
			#sortitem { tag=parent3, body=#panel { body=[
				#span { class=handle, text="(Grab)" }, " Parent 3",
				#sortblock { postback=child3sort, body=[
					#sortitem { tag=child3_1, body="Child 1" },
					#sortitem { tag=child3_2, body="Child 2" },
					#sortitem { tag=child3_3, body="Child 3" },
					#sortitem { tag=child3_4, body="Child 4" }
				]}
			]}}
		]}

	]},
	wf:render(Body).
	

	
sort_event(_Tag, Items) -> 
	Message = wf:f("Order: ~p", [Items]),
	wf:wire(#alert { text=Message }),
	ok.

event(_) -> ok.