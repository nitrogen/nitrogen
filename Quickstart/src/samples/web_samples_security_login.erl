-module(web_samples_security_login).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> 
    #template { file="./templates/onecolumn.html", bindings=[
        {'Group', learn},
        {'Item', samples}
    ]}.

title() -> "Security - Login".
headline() -> "Security - Login".
right() -> linecount:render().

body() -> 
    wf:wire(okButton, userTextBox, #validate { validators=[
        #is_required { text="Required" }
    ]}),
    wf:wire(okButton, passTextBox, #validate { validators=[
        #is_required { text="Required" },
        #custom { text="Hint: password is 'secret'.", function=fun check_password/2 }
    ]}),
    [
        "
        Enter a username and the password \"secret\" to log in.
        ",
        #label { text="Username" },
        #textbox { id=userTextBox, next=passTextBox },
        #p{},
        #label { text="Password" },
        #password { id=passTextBox, next=okButton },
        #p{},
        #button { id=okButton, text="OK", postback=ok }
    ].

check_password(_Tag, Value) ->
    Value == "secret".

event(ok) ->
    User = wf:q(userTextBox),
    wf:user(User),
    wf:redirect_from_login("/web/samples/security").
