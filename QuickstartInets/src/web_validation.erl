-module (web_validation).
-include ("wf.inc").
-include ("google_chart.inc").
-export ([main/0, event/1]).

main() ->
	Body = #body { title="Validation", body=
		#panel { id=mainPanel, style="padding: 20px;", body=[
			#h1 { text="Validation Sample" },
			#flash {},
			#label { text="Name" },
			#textbox { id=nameTextBox, next=emailTextBox },
			#p{},
			
			#label { text="Email Address" },
			#textbox { id=emailTextBox, next=passwordTextBox },
			#p{},
			
			#label { text="Password" },
			#textbox { id=passwordTextBox, next=confirmTextBox },
			#p{},
			
			#label { text="Confirm" },
			#textbox { id=confirmTextBox, next=otherTextBox },
			#p{},
			
			#label { text="Other" },
			#textbox { id=otherTextBox, next=continueButton },
			#p{},
			
			#button { id=continueButton, text="Continue", postback=continue }
		]}
	},
	
	wf:wire(mainPanel.continueButton, nameTextBox, #validate { validators=[
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
	?PRINT(Value),
	Value1 = string:to_lower(Value),
	case Value1 of 
		"rusty" ++ _ -> true;
		_ -> false
	end.