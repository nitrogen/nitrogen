-module (web_sorting2).
-include ("wf.inc").
-export ([main/0, event/1, sort_event/2]).

main() ->
	Body = #body { title="Nested Sorting Example", body=
		#panel { style="padding: 20px;", body=[
			#h1 { text="Nested Sorting Example" },
			#p{},			
			#panel { style="width: 200px; float: left;", body=[
				#h3 { text="Block 1" },
				#sortblock { postback=sort, handle=grab, body=[
					#sortitem { tag=tag1, body=#panel { body=[
						#span { class=grab, text="(Grab)" }, " Parent 1",
						#sortblock { style="margin-left: 20px;", postback=child1sort, body=[
							#sortitem { tag=tag1_1, body="Child 1" },
							#sortitem { tag=tag1_2, body="Child 2" },
							#sortitem { tag=tag1_3, body="Child 3" },
							#sortitem { tag=tag1_4, body="Child 4" }
						]}
					]}},
					#sortitem { tag=tag2, body=#panel { body=[
						#span { class=grab, text="(Grab)" }, " Parent 2",
						#sortblock { style="margin-left: 20px;", postback=child2sort, body=[
							#sortitem { tag=tag2_1, body="Child 1" },
							#sortitem { tag=tag2_2, body="Child 2" },
							#sortitem { tag=tag2_3, body="Child 3" },
							#sortitem { tag=tag2_4, body="Child 4" }
						]}
					]}},
					#sortitem { tag=tag3, body=#panel { body=[
						#span { class=grab, text="(Grab)" }, " Parent 3",
						#sortblock { style="margin-left: 20px;", postback=child3sort, body=[
							#sortitem { tag=tag3_1, body="Child 1" },
							#sortitem { tag=tag3_2, body="Child 2" },
							#sortitem { tag=tag3_3, body="Child 3" },
							#sortitem { tag=tag3_4, body="Child 4" }
						]}
					]}}
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