-module (web_samples_notices).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/onecolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "User Notices".
headline() -> "User Notices". 
right() -> linecount:render().

body() -> [
	#p{},
	"
		Nitrogen provides you with a variety of ways to indicate
		simple messages to a user.
	",
	
	#p{},
	#button { text="Show Flash Message", postback=show_flash },

	#p{},
	#button { text="Show Advanced Flash Message", postback=show_advanced_flash },

	#p{},	
	#button { text="Show Javascript Alert", postback=show_alert },

	#p{},	
	#button { text="Show Javascript Confirm", postback=show_confirm }	
].
	
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