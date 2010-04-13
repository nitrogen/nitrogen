-module (demos_ajax).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "Ajax".

headline() -> "Ajax".

left() -> 
    [
        "
        <p> 
        Upon receiving a postback event, your application can
        manipulate the content of elements on the page via calls to
        <code>wf:update/2</code>, <code>wf:insert_top/2</code>, and
        <code>wf:insert_bottom/2</code>.

        <p>
        In this demo, the 'Go' button is wired to update the contents of
        the columns below it.  
        ", 
        linecount:render() 
    ].

right() -> 
    [
	% Set up the form...
	#textbox { id=theMessage, text="Hello, world!", next=theButton },
	#button { id=theButton, text="Go", postback=click },
	#p{},	

	% Create a table with three columns...
	#table { style="width: 100%;", rows=[
            #tablerow { cells=[
                #tableheader { style="width: 33%;", text="Update" },
                #tableheader { style="width: 33%;", text="Insert Top" },
                #tableheader { style="width: 33%;", text="Insert Bottom" }
            ]},
            #tablerow { cells=[
                #tablecell { id=updateCell },
                #tablecell { id=topCell },
                #tablecell { id=bottomCell }
            ]}
	]}
    ].

event(click) ->
    % Get the message...
    Message = wf:q(theMessage),

    % Replace old contents of this cell...
    wf:update(updateCell, #panel { body=Message }),

    % Insert at the top of this cell...
    wf:insert_top(topCell, #panel { body=Message, actions=#show { effect=puff }}),

    % Insert at the bottom of this cell...
    wf:insert_bottom(bottomCell, #panel { body=Message, actions=#show { effect=pulsate, options=[{times, 1}] }}),
    ok;	

event(_) -> ok.
