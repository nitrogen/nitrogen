-module(web_samples_security_restricted).
-include ("wf.inc").
-compile(export_all).

main() -> 
    case wf:user() /= undefined of 
        true  -> main_authorized();
        false -> wf:redirect_to_login("/web/samples/security/login")
    end.

main_authorized() ->
    #template { file="./wwwroot/onecolumn.html", bindings=[
        {'Group', learn},
        {'Item', samples}
    ]}.

title() -> "Security - Restricted Page".
headline() -> "Security - Restricted Page".
right() -> linecount:render().

body() -> 
    [
        "
        Congratulations, if you can see this page, then you are logged in.
        <p>
        If you try to navigate to this page while you are NOT logged
        in, then the page uses the wf:redirect_to_login(Url) function
        to send you off to a login page. Once you have authenticated,
        the login page uses wf:redirect_from_login(DefaultUrl), which
        redirects back to the original URL, or redirects to the
        DefaultUrl if the user hit the login page directly.
        <p>
        ",
        #link { text="Go Back", url="/web/samples/security" }
    ].
