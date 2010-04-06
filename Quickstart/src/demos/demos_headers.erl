-module (demos_headers).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> 
    #template { file="./templates/demos46.html" }.

title() -> "Headers".

headline() -> "Headers".

left() -> 
    [
        "
        <p>
        Nitrogen allows you to access http headers via the
        <code>wf:headers/0</code> and <code>wf:headers/1</code>
        functions.
        ",
        linecount:render()
    ].


right() -> 
    Headers = [{wf:to_list(Key), wf:to_list(Value)} || {Key, Value} <- wf:headers()],

    [
        #table { rows=[
            #tablerow { cells=[
                #tableheader { text="Header", style="width: 120px; background-color: #ddd;" },
                #tableheader { text="Value", style="background-color: #ddd;" }
            ]},
            #bind {
                data=Headers,
                map={headerCell@text, valueCell@text},
                body=#tablerow { cells=[
                    #tablecell { id=headerCell, style="background-color: #eee; font-size: .8em;" }, 
                    #tablecell { id=valueCell, style="font-size: .8em;" }
                ]}
            }
        ]}
    ].

event(_) -> ok.
