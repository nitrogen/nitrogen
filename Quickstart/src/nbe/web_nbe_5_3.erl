-module (web_nbe_5_3).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/nbe.html" }.	

body() -> 
	Terms = case wf:user() of
		undefined -> show_login_panel();
		_ -> show_logout_panel()
	end,
	#panel { id=mainPanel, body=Terms }.
	
show_login_panel() ->
	#panel { body=[
		#label { text="Username" },
		#textbox { id=usernameTextBox, next=loginButton },
		#p{},
		#checkbox { id=adminCheckbox, text="Grant administrator access" },
		#p{},
		#button { id=loginButton, text="Login", postback=login }
	]}.
	
show_logout_panel() ->
	{user, Name} = wf:user(),
	IsAdmin = wf:role(admin),
	#panel { body=[
		#span { text=wf:f("You are logged in as '~s'.", [Name]) },
		#p{},
		#span { show_if=IsAdmin, text="You are an administrator." },
		#span { show_if=not IsAdmin, text="You are not an administrator." },
		#p{},
		#button { text="Logout", postback=logout }
	]}.
	
event(login) ->
	[User] = wf:q(usernameTextBox),
	HasAdminRole = wf:q(adminCheckbox) /= [],
	wf:user({user, User}),
	wf:role(admin, HasAdminRole),
	wf:update(mainPanel, show_logout_panel());
	
event(logout) ->
	wf:logout(),
	wf:update(mainPanel, show_login_panel	()).
	
	
