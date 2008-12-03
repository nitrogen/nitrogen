-module (web_holidaydemo).
-include ("wf.inc").
-export ([main/0, event/1]).

-record(item, { 

main() ->
	Data = [
		"Partridge in a Pear Tree"
		"Turtle Dove"
		"French Hen"
		"Calling Bird"
		"Golden Ring"
		"Laying Goose"
		"Swimming Swan"
		"Milking Maid"
		"Dancing Lady"
		"Leaping Lord"
		"Piper"
		"Drummer"
	],

	Body = #body { title="Holiday Demo", body=
		#panel { style="padding: 20px;", body=[
			#h1 { text="Holiday Demo" },
			
			#flash {},
			
			#button { text="Show Flash Message", postback=show_flash },
			#p{},
			
			#button { text="Show Javascript Alert", postback=show_alert },
			#p{},
			
			#button { text="Show Javascript Confirm", postback=show_confirm }	
		]}
	},
	wf:render(Body).
	
event(show_flash) ->
	wf:flash("This is a flash message.");
	
event(show_alert) ->
	wf:wire(#alert { text="This is a Javascript Alert" });
	
event(show_confirm) ->
	wf:wire(#confirm { text="This is a Javascript Confirm", postback=confirm_ok });
	
event(confirm_ok) ->
	wf:wire(#alert { text="You pressed the OK button." });
	
event(_) -> ok.
