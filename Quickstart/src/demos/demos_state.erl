-module (demos_state).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "Session State and Page State".

headline() -> "Session State and Page State".

left() -> 
    [
        "
        <p>
        The first counter to the right is stored in session. If you reload
        the current page, or open a new tab in your browser and
        navigate to this window, the count is persisted. (It will
        disappear once the session times out.)

        <p>
        The second counter is stored in page state. It exists only for
        postbacks on the current page, and its value disappears when
        you close the page.
        ",
        linecount:render()
    ].

right() -> 
    Elements = [
        #p{},
        #p{},
        #span { text="Counter stored in session state: " }, #span { id=sessionStateCountSpan },
        #p {},
        #span { text="Counter stored in page state: " }, #span { id=pageStateCountSpan },
        #p {},
        #button { text="Increment Counters", postback=increment }
    ],
    
    % Update the elements with their values...
    update_elements(),
    Elements.

	
update_elements() ->
    % Update the sessionStateCountSpan element.
    SessionStateCount = wf:coalesce([wf:session(count), 1]),
    wf:set(sessionStateCountSpan, SessionStateCount),

    % Update the pageStateCountSpan element...
    PageStateCount = wf:coalesce([wf:state(count), 1]),
    wf:set(pageStateCountSpan, PageStateCount).


event(increment) ->
    SessionCount = wf:coalesce([wf:session(count), 1]),
    StateCount = wf:coalesce([wf:state(count), 1]),
    wf:session(count, SessionCount + 1),
    wf:state(count, StateCount + 1),
    update_elements(),
    ok.
