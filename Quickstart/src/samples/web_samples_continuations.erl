-module (web_samples_continuations).
-include ("wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/onecolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "Continuations".
headline() -> "Continuations".
right() -> linecount:render().

body() -> [
	#p{},
	"
		A Nitrogen continuation lets you kick off a long running-function
		on the server and have the browser periodically check in
		to see if it is finished.
		<p>
		You can also set a timeout, where the server basically just throws
		up it's hands after X seconds.
	",
	
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
	wf:continue({continue, Description}, F, 100, TimeoutSeconds * 1000);

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
	