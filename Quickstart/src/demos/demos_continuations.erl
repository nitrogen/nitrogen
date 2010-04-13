-module (demos_continuations).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "Continuations".

headline() -> "Continuations".

left() -> 
    [
	"
        <p>
        Nitrogen supports some (possibly-confusingly named)
        functionality called continuations. A Nitrogen continuation
        allows you to kick off a function, and then receive a Nitrogen
        postback once that function has completed, where the results
        of the function are possed to the postback.

        <p>
        Additionally, Nitrogen continuations support a timeout
        parameter. When the timeout is exceeded, the postback fires
        with a result of 'timeout'.

        <p>
        Click the buttons below to spawn a few long running
        processes. (They simply sleep for a few seconds and then
        return.)
	",

        linecount:render()
    ].

right() -> [
    #flash{},

    #p{},
    #button { text="Start a 1 second task.", postback={continue, "1 Second Task", 1, 20} },

    #p{},	
    #button { text="Start a 3 second task.", postback={continue, "3 Second Task", 3, 20} }, 

    #p{},	
    #button { text="Start a 5 second task.", postback={continue, "5 Second Task", 5, 20} }, 

    #p{},
    #button { text="Start a 60 second task that times out after 3 seconds.", postback={continue, "60 Second Task", 100, 3} }
].

event({continue, Description, DelaySeconds, TimeoutSeconds}) ->
    wf:flash("Started the task..."),
    F = fun() -> long_running_function(DelaySeconds) end,
    wf:continue({continue, Description}, F, TimeoutSeconds * 1000);

event(_) -> ok.

continue({continue, Description}, timeout) ->
    Message = wf:f("Task '~s' timed out.", [Description]),
    wf:flash(Message);

continue({continue, Description}, _Result) ->
    Message = wf:f("Finished the task: '~s'", [Description]),
    wf:flash(Message).

long_running_function(Duration) ->
    % Do a lot of hard work here.
    receive 
    after Duration * 1000 -> ok
    end.

