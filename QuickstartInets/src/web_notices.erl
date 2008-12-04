-module (web_notices).
-include ("wf.inc").
-export ([main/0, event/1]).

main() ->
	Title = "User Notices",
	Body = #template { file=onecolumn, title=Title, headline=Title, section1=[

		"
			Nitrogen provides you with a variety of ways to indicate
			simple messages to a user.
		",
		#p{},			
		#flash {},
		#p{},
		#button { text="Show Flash Message", postback=show_flash },
		#p{},

		#button { text="Show Advanced Flash Message", postback=show_advanced_flash },
		#p{},
		
		#button { text="Show Javascript Alert", postback=show_alert },
		#p{},
		
		#button { text="Show Javascript Confirm", postback=show_confirm }	
	]},
	wf:render(Body).
	
event(show_flash) ->
	wf:flash("This is a flash message.");

event(show_advanced_flash) ->
	wf:flash([
		#span { text="Flash messages can contain nested controls." },
		#p{},
		#button { text="Click Me", postback=advanced_flash_click }
	]);
	
event(advanced_flash_click) ->
	wf:flash("You clicked the button.");
	
event(show_alert) ->
	wf:wire(#alert { text="This is a Javascript Alert" });
	
event(show_confirm) ->
	wf:wire(#confirm { text="This is a Javascript Confirm", postback=confirm_ok });
	
event(confirm_ok) ->
	wf:wire(#alert { text="You pressed the OK button." });
	
event(_) -> ok.