-module(web_samples_security).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/onecolumn.html", bindings=[
    {'Group', learn},
    {'Item', samples}
]}.

title() -> "Security".
headline() -> "Security".
right() -> linecount:render().

body() -> 
    CurrentUser = case wf:user() of
        undefined -> "(Anonymous)";
        Other -> Other
    end,            
    [
        "
        Nitrogen has convenience methods for storing user information 
        and roles. See wf:user/N, and wf:role/N. 
        <p>
        In addition, Nitrogen provides methods to validate that the
        user has access to a page, redirect to a login page, and then
        redirect back to the original page.
        <p>
        ",
        #span { text="Current User: " },
        #span { style="font-weight: bold;", text=CurrentUser }, 
        #p{},
        #link { show_if=(wf:user() /= undefined), text="Logout", postback=logout },
        #p{},
        #link { text="Access a Restricted Page", url="/web/samples/security/restricted" },
        "
        <p>
        Only authenticated users can access the restricted page. Click
        on the link below. If you are currently an anonymous user, you
        will be prompted for a username and password.
        "
    ].

event(logout) -> 
    wf:clear_session(),
    wf:redirect("/web/samples/security").
