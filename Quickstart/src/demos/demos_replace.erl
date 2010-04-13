-module (demos_replace).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "AJAX Replace".

headline() -> "AJAX Replace".

left() -> 
    [
        "
        <p>
        Nitrogen allows you to replace elements on the client
        page. When you click on the 'Replace' button, the page will
        use <code>wf:replace(mainPanel, Message)</code> to replace the
        contents of the panel.

        <p>
        Unlike <code>wf:update/2</code> (which replaces just the inner
        contents of an element), the <code>replace/2</code> function
        replaces the entire DOM element.
        ",
         linecount:render()
    ].

right() ->
    [
        #panel { id=mainPanel, body=[
            "This panel will be replaced.",
            #p{},
            #button { text="Replace", postback=replace }
        ]}
    ].

event(replace) ->
    Elements = #panel { id=replacementPanel, body=[
        "See!",
        #p{},
        #button { text="Reset", postback=reset }
    ]},
    wf:replace(mainPanel, Elements);

event(reset) ->
    wf:redirect("/demos/replace");

event(_) -> ok.
