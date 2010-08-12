-module (demos_comet3).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.
	
title() -> "Multiple Comet Processes".

headline() -> "Multiple Comet Processes".

left() -> 
    [
        "
        <p>
        This demo shows how to run multiple Comet processes at
        once. Nitrogen automatically merges the results of the Comet
        processes together into a single stream to the client, so the
        only real limit on the number of Comet processes is server
        resources.

        <p>
        In addition, this demo shows how to detect when a user has 
        left the page, shutting the Comet process down gracefully.

        <p>
        Here, we simply cycle through some colors and cardinal
        directions at different intervals.
        ",
        linecount:render()
    ].

right() -> 
    Body = [
        #span { text="Cycle through colors: " },
        #span { id=colorLabel, text="-" },
        #p{},
        #span { text="Cycle through directions: " },
        #span { id=directionLabel, text="-" }
    ],
    
    % Start the counter as a background process.
    wf:comet(fun() -> cycle_and_update(1000, colorLabel, ["Red", "Orange", "Yellow", "Green", "Blue", "Indigo", "Violet"]) end),
    wf:comet(fun() -> cycle_and_update(2000, directionLabel, ["North", "East", "South", "West"]) end),
    Body.

event(_) -> ok.

cycle_and_update(Speed, ControlID, List) ->
    % Don't exit on error...
    process_flag(trap_exit, true),

    % Check to see if we have received an EXIT notice.
    receive 
        {'EXIT', _, Message} -> 
            ?PRINT(Message),
            io:format("The user has left the page.~n"),
            exit(done)
    after 0 -> continue
    end,

    % Sleep for a second, then update
    timer:sleep(Speed),

    % Update the control.
    wf:update(ControlID, hd(List)),

    % wf:comet_flush() is only needed because we are looping. Otherwise,
    % we could just let the function complete.
    wf:flush(),

    % Take the first item from the list, make it the last item on the list.
    % So if we start with [1, 2, 3, 4], we'd end with [2, 3, 4, 1]
    List1 = tl(List) ++ [hd(List)],

    % Loop. This process will automatically be killed
    % once the page stops requesting the output that
    % it generates.
    cycle_and_update(Speed, ControlID, List1).

