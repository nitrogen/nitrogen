-module(demos_security_login).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> 
    #template { file="./templates/demos46.html" }.

title() -> "Security - Login".

headline() -> "Security - Login".

left() -> 
    [
        "
        <p>
        If you are on this page, you most likely were redirected here
        from the page located at
        <i>/demos/security/restricted</i>. That page used the
        <code>wf:redirect_to_login/1</code> function to serialize the
        requested page and then kick you off to the login page.

        <p>
        Enter a username and password 'secret' in the form on the
        right. When you press 'OK', the page will use the
        <code>wf:redirect_to_login/1</code> function to try to
        redirect you back to the page you originally requested.
        ",
        linecount:render()
    ].

right() -> 
    wf:wire(okButton, userTextBox, #validate { validators=[
        #is_required { text="Required" }
    ]}),
    wf:wire(okButton, passTextBox, #validate { validators=[
        #is_required { text="Required" },
        #custom { text="Hint: password is 'secret'.", function=fun check_password/2 }
    ]}),
    [
        #p{},
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
