-module (demos_notices).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "User Notices".

headline() -> "User Notices". 

left() -> 
    [
        "
        <p>
        Nitrogen provides you with a variety of ways to notify a user
        of information.

        <p>
        <h2>Flash Message</h2>

        <p>
        A flash message is an HTML box that slides into the page at a
        point specified by the <code>#flash{}</code> element. The flash
        message can be a simple text-message, or can contain other controls. 
        To display a flash message, call <code>wf:flash(TextOrElements)</code>.
        
        <p>
        Flash messages are stored in session until they are displayed. This
        allows your application to set a flash message on one page in response 
        to user action and then redirect the user to another page, at which 
        the message is displayed.

        <p>
        <h2>Alert Box</h2>
        
        <p>
        The <code>#alert {}</code> action displays a simple
        Javascript alert to the user.

        <p>
        <h2>Confirm Box</h2>

        <p>
        The <code>#confirm {}</code> action displays a Javascript
        confirm box to the user, and posts back if the 'OK' button is
        clicked.
        ",
        linecount:render()
    ].

right() -> 
    [
        #flash{},

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
    FlashID = wf:temp_id(),
    wf:flash(FlashID, [
        #span { text="Flash messages can contain nested controls." },
        #p{},
        #button { text="Click Me", postback={advanced_flash_click, FlashID} }
    ]);

event({advanced_flash_click, FlashID}) ->
    wf:flash("You clicked the button."),
    wf:wire(FlashID, #hide { effect=blind, speed=100 });

event(show_alert) ->
    wf:wire(#alert { text="This is a Javascript Alert" });

event(show_confirm) ->
    wf:wire(#confirm { text="This is a Javascript Confirm", postback=confirm_ok });

event(confirm_ok) ->
    wf:wire(#alert { text="You pressed the OK button." });

event(_) -> ok.
