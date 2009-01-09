-module (web_samples_sorting1).
-include ("wf.inc").
-compile(export_all).

main() ->	#template { file="./wwwroot/twocolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "Simple Sorting".
headline() -> "Simple Sorting".
right() -> linecount:render().

column1() -> [
	#h3 { text="Block 1" },
	#sortblock { class=simple, group=block1, style="padding: 10px; background-color: #eee;", connect_with_groups=block2, tag=sort, items=[
		#sortitem { tag=tag1, body="Item1" },
		#sortitem { tag=tag2, body="Item2" },
		#sortitem { tag=tag3, body="Item3" },
		#sortitem { tag=tag4, body="Item4" }
	]}
].

column2() -> [
	#h3 { text="Block 2" },
	#sortblock { class=simple, group=block2, style="padding: 10px; background-color: #eee;", connect_with_groups=block1, tag=sort, items=[
		#sortitem { tag=tag5, body="Item5" },
		#sortitem { tag=tag6, body="Item6" },
		#sortitem { tag=tag7, body="Item7" },
		#sortitem { tag=tag8, body="Item8" }
	]}
].
	
sort_event(_Tag, Items) -> 
	Message = wf:f("Order: ~p", [Items]),
	wf:wire(#alert { text=Message }),
	ok.

event(_) -> ok.