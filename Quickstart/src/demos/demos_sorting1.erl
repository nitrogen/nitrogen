-module (demos_sorting1).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() ->  #template { file="./templates/demos433.html" }.

title() -> "Sorting".

headline() -> "Sorting".

left() -> 
    [
        "
        <p>
        The <code>#sortblock{}</code> and <code>#sortitem{}</code> elements
        allow you to specify elements that should be sortable. 

        <p> 
        Both the block and the items themselves can have an associated
        tag. When a sort event occurs on the page, Nitrogen posts back the
        tags in their new sorted order.

        ",
        linecount:render()
    ].

middle() -> 
    [
	#h3 { text="Block 1" },
	#sortblock { class=simple, group=block1, style="padding: 10px; background-color: #eee;", connect_with_groups=block2, tag=sort, items=[
            #sortitem { tag=tag1, body="Item1" },
            #sortitem { tag=tag2, body="Item2" },
            #sortitem { tag=tag3, body="Item3" },
            #sortitem { tag=tag4, body="Item4" }
	]}
    ].

right() ->
    [
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
