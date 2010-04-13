-module (demos_remove).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "AJAX Remove".

headline() -> "AJAX Remove".

left() -> 
    [
        "
        <p>
        Nitrogen allows you to remove elements from the client
        page. When you click the 'Remove' button, the page will call
        <code>wf:remove/1</code> to remove the panel.

        <p>
        Reload the page to reset.
        ",
        linecount:render()
    ].

right() -> 
    [
	#panel { id=mainPanel, body=[
            "This panel will be removed.",
            #p{},
            #button { text="Remove", postback=remove }
        ]}
    ].

event(remove) ->
    wf:remove(mainPanel);

event(_) -> ok.
