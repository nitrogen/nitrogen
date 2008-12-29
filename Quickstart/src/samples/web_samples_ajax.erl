-module (web_samples_ajax).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/onecolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "Ajax Example".
headline() -> "Ajax Example".
right() -> linecount:render().

body() -> [
	% Set up the form...
	#p{},
	#textbox { id=theMessage, text="This is a message...", next=theButton },
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
	[Message] = wf:q(theMessage),

	% Replace old contents of this cell...
	wf:update(updateCell, #panel { body=Message }),
	
	% Insert at the top of this cell...
	wf:insert_top(topCell, #panel { body=Message, actions=#show { effect=puff }}),

	% Insert at the bottom of this cell...
	wf:insert_bottom(bottomCell, #panel { body=Message, actions=#show { effect=pulsate, options=[{times, 1}] }}),
	ok;	

event(_) -> ok.