-module (web_nbe_4_1).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/nbe.html" }.	

body() -> 
	[
		#sortblock { tag=sort, items=[
			#sortitem { tag=1, body="Sort Me 1" },
			#sortitem { tag=2, body="Sort Me 2" },
			#sortitem { tag=3, body="Sort Me 3" },
			#sortitem { tag=4, body="Sort Me 4" }
		]},
	#p{},
	#span { id=myspan }
	].
	
sort_event(sort, ItemTags) ->
	S = wf:f("The order is: ~p", [ItemTags]),
	wf:update(myspan, S).