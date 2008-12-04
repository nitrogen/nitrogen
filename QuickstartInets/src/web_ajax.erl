-module (web_ajax).
-include ("wf.inc").
-export ([main/0, event/1]).

main() ->
	Title = "Ajax Example",
	Body = #template { file=onecolumn, title=Title, headline=Title,
	section1=[

		% Set up the form...
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
		
	]},
	wf:render(Body).
	
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