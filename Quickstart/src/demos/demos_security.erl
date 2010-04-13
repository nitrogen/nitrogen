-module(demos_security).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "Security".

headline() -> "Security".

left() -> 
    [
        "
        <p>
        Nitrogen has convenience methods for storing user information
        and roles. See <code>wf:user/N</code>, and <code>wf:role/N</code>.

        <p>
        In addition, Nitrogen provides methods to validate that the
        user has access to a page, redirect to a login page, and then
        redirect back to the original page.

        <p> 
        In this demo, only authenticated users can access the
        restricted page. If you are currently an anonymous user, you
        will be prompted for a username and password.
        ",
        linecount:render()
    ].


right() -> 
    CurrentUser = case wf:user() of
        undefined -> "(Anonymous)";
        Other -> Other
    end,            
    [
        #span { text="Current User: " },
        #span { style="font-weight: bold;", text=CurrentUser }, 
        #p{},
        #link { show_if=(wf:user() /= undefined), text="Logout", postback=logout },
        #p{},
        #link { text="Access a Restricted Page", url="/demos/security/restricted" }
    ].

event(logout) -> 
    wf:clear_session(),
    wf:redirect("/demos/security").
