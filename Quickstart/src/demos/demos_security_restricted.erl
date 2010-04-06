-module(demos_security_restricted).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> 
    case wf:user() /= undefined of 
        true  -> main_authorized();
        false -> wf:redirect_to_login("/demos/security/login")
    end.

main_authorized() -> #template { file="./templates/demos46.html" }.

title() -> "Security - Restricted Page".

headline() -> "Security - Restricted Page".

left() -> 
    [
        "
        <p>
        Congratulations, if you can see this page, then you are logged
        in.

        <p>
        If you try to navigate to this page while you are NOT logged
        in, then the page uses the <code>wf:redirect_to_login/1</code>
        function to send you off to a login page. Once you have
        authenticated, the login page uses
        <code>wf:redirect_from_login/1</code>, which redirects back to
        the original URL, or redirects to the specified DefaultUrl if
        the user hit the login page directly.
        ",
        linecount:render()
    ].

right() -> 
    [
        #p{},
        " 
	TOP SECRET SUPER SECURE INFORMATION.  
	TOP SECRET SUPER SECURE INFORMATION.  
	TOP SECRET SUPER SECURE INFORMATION.  
	TOP SECRET SUPER SECURE INFORMATION.  
	TOP SECRET SUPER SECURE INFORMATION.  
	TOP SECRET SUPER SECURE INFORMATION.  
	TOP SECRET SUPER SECURE INFORMATION.  
	TOP SECRET SUPER SECURE INFORMATION.  
	TOP SECRET SUPER SECURE INFORMATION.  
	TOP SECRET SUPER SECURE INFORMATION.  
	TOP SECRET SUPER SECURE INFORMATION.  
	TOP SECRET SUPER SECURE INFORMATION.  
	TOP SECRET SUPER SECURE INFORMATION.  
        ",
        #p{},
        #link { text="Go Back", style="font-size: 200%;", url="/demos/security" }
    ].
