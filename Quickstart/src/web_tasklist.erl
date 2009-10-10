-module (web_tasklist).
-include ("wf.inc").
-compile(export_all).

-record(task, { title }).

main() -> 
	#template { file="./wwwroot/tasklist.html" }.

title() -> "Task List Example".

body() ->
	wf:wire(#validate { trigger=createButton, target=titleTextBox, attach_to=createButton, validators=[
			#is_required { text="Required!" }
	]}),
	
	[
		#h1 { text="My Business Plan" },
		#sortblock { id=taskList, tag=mysortblock, items=[render_task(X) || X <- taskdata:get_tasks()] },
		#textbox { id=titleTextBox, next=createButton },
		#button { id=createButton, text="Create Task", postback=create },
		#p{},
		#link { text="Use default business plan", style="font-size: 70%;", postback=bizplan }
	].
	
render_task(Task) ->
	TaskID = wf:temp_id(),
	SpanID = wf:temp_id(),
	TextBoxID = wf:temp_id(),
	#sortitem {
		id=TaskID,
		tag=Task,
		class=task,
		body=[
			#link { text="Delete", postback={delete, TaskID, Task} },
			#textbox { id=TextBoxID, text=Task#task.title, style="display: none;", postback={save, SpanID, TextBoxID, Task} },
			#span { id=SpanID, text=Task#task.title }
		],
		actions=[
			#effect { effect=highlight },
			#event { type=dblclick, postback={edit, SpanID, TextBoxID} }
		]		
	}.
	
sort_event(mysortblock, Tasks) ->
	taskdata:save_tasks(Tasks).
	
event(bizplan) ->
	Tasks = [
		#task { title="Step 1 - Write an Erlang application" },
		#task { title="Step 2 - Profit" },
		#task { title="Step 3 - Generously donate to charity" }
	],
	taskdata:save_tasks(Tasks),
	RenderedTasks = [render_task(X) || X <- taskdata:get_tasks()],
	wf:update(taskList, RenderedTasks);
	
	
event({save, SpanID, TextBoxID, OldTask}) ->
	wf:wire(TextBoxID, #hide{}),
	wf:wire(SpanID, #appear{}),
	Title = wf:q(TextBoxID),
	wf:update(SpanID, Title),
	NewTask = OldTask#task { title=Title },
	Tasks = taskdata:get_tasks(),
	Tasks1 = taskdata:replace(OldTask, NewTask, Tasks),
	taskdata:save_tasks(Tasks1);
	
event({edit, SpanID, TextBoxID}) ->
	wf:wire(SpanID, #hide {}),
	wf:wire(TextBoxID, #appear {}),
	wf:wire(TextBoxID, "obj('me').select();");
	
event({delete, TaskID, Task}) ->
	Tasks = taskdata:get_tasks(),
	taskdata:save_tasks(Tasks -- [Task]),
	wf:wire(TaskID, #fade{});
	
event(create) -> 
	Title = wf:q(titleTextBox),
	Task = #task { title=Title },
	Tasks = taskdata:get_tasks(),
	taskdata:save_tasks(Tasks ++ [Task]),
	wf:insert_bottom(taskList, render_task(Task)),
	wf:set(titleTextBox, ""),
	wf:wire(titleTextBox, "obj('me').select();");

event(_Tag) -> ok.