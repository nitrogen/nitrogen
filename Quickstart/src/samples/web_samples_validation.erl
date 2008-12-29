-module (web_samples_validation).
-include ("wf.inc").
-include ("google_chart.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/onecolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "Validation".
headline() -> "Validation". 
right() -> linecount:render().

body() -> 
	Body = [
		#p{},
		#label { text="Name" },
		#textbox { id=nameTextBox, next=emailTextBox },
		
		#p{},
		#label { text="Email Address" },
		#textbox { id=emailTextBox, next=passwordTextBox },

		#p{},	
		#label { text="Password" },
		#password { id=passwordTextBox, next=confirmTextBox },

		#p{},	
		#label { text="Confirm" },
		#password { id=confirmTextBox, next=otherTextBox },

		#p{},	
		#label { text="Other" },
		#textbox { id=otherTextBox, next=continueButton },

		#p{},	
		#button { id=continueButton, text="Continue", postback=continue }
	],
	
	wf:wire(continueButton, nameTextBox, #validate { validators=[
		#is_required { text="Required." },
		#custom { text="Must start with 'Rusty'.", tag=some_tag, function=fun custom_validator/2 }
	]}),
	
	wf:wire(continueButton, emailTextBox, #validate { validators=[
		#is_required { text="Required." },
		#is_email { text="Enter a valid email address." }
	]}),

	wf:wire(continueButton, passwordTextBox, #validate { validators=[
		#is_required { text="Required." },
		#min_length { length=6, text="Password must be at least 6 characters long." }
	]}),

	wf:wire(continueButton, confirmTextBox, #validate { validators=[
		#is_required { text="Required." },
		#confirm_password { password=passwordTextBox, text="Passwords must match." }
	]}),	
		
	wf:render(Body).
	
event(continue) ->
	[Name] = wf:q(nameTextBox),
	Message = wf:f("Welcome ~s! Thank you for signing up.", [Name]),
	wf:flash(Message),
	ok;
	
event(_) -> ok.

custom_validator(_Tag, Value) ->
	Value1 = string:to_lower(Value),
	case Value1 of 
		"rusty" ++ _ -> true;
		_ -> false
	end.