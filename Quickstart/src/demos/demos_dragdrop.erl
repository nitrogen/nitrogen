-module (demos_dragdrop).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "Drag and Drop".

headline() -> "Drag and Drop".

left() -> 
    [
        "
        <p>
        The <code>#draggable{}</code> and <code>#droppable{}</code>
        elements allow you to mark a block of elements as draggable or
        droppable. 

        <p>
        Both the draggable object and the droppable object have
        associated tags.  When a drop event occurs, Nitrogen posts
        back a drop_event with the associated tag information,
        allowing your application to respond.

        ",
        linecount:render()
    ].

right() -> 
    [
        #flash{},

        #p{},
        #draggable { group=group1, tag=drag1, clone=true, body="Drag 1<br>Clone<br>Revert"	},
        #draggable { group=group1, tag=drag2, clone=true, revert=false, body="Drag 2<br>Clone<br>No Revert"	},
        #draggable { group=group1, tag=drag3, clone=false, revert=true, body="Drag 3<br>No Clone<br>Revert"	},
        #draggable { group=group1, tag=drag4, clone=false, revert=false, body="Drag 4<br>No Clone<br>No Revert"	},
        #draggable { group=group2, tag=drag5, handle=handle, body=[
            #span { class=handle, text="(Handle)" },
            "<BR>",
            "Drag 5"
        ]},
        #p{},
        #droppable { accept_groups=group1, tag=drop1, body="Drop 1 (accepts Drag 1 to 4)" },
        #droppable { accept_groups=[group1, group2], tag=drop2, body="Drop 2 (accepts everything)" }
    ].	

drop_event(DragTag, DropTag) ->
    Message = wf:f("Dropped ~p on ~p", [DragTag, DropTag]),
    wf:flash(Message),
    ok.

event(_) -> ok.
