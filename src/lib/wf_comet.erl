-module (wf_comet).
-include ("wf.inc").
-export ([
	comet/1,
	comet_flush/0,
	get_content/0
]).

-define(CONTENT_LOOP_INTERVAL, 300).
-define(COMET_REQUEST_TIMEOUT, 20000).
-define(INACTIVITY_TIMEOUT, 20000).

comet(Function) -> comet(long_poll, Function).

comet(Type, Function) ->
	% Make sure the comet loop is running...
	ensure_comet_loop(?COMET_REQUEST_TIMEOUT),
	
	CurrentState = get(),
	F = fun() ->
		% Copy state to the new process...
		[put(X, Y) || {X, Y} <- CurrentState],
		reset_nitrogen_state(),		
		
		% HACK - We don't know if the page has flash or not,
		% but assume it does, because if a comet function is
		% writing to flash, then 99% of the time it will.
		wf:state(has_flash, true),
		
		% Link to wf_comet_pid so that we don't run if it's down...
		CometPid = wf:state(wf_comet_pid),
		erlang:link(CometPid),
		
		% Run the function...
		try Function() 
		catch Type : Error ->
			io:format("CAUGHT ERROR: ~p-~p~n~p~n", [Type, Error, erlang:get_stacktrace()])
		end,
		
		% Send the output to the comet middleman...
		comet_flush()
	end,
	spawn(F).
	
comet_flush() ->
	% Process any flash events...
	element_flash:update(),

	% Get the script...
	Content = wf_script:get_script(false),

	% See if there are redirects...
	RedirectUrl = get(wf_redirect),
	IsRedirect = RedirectUrl /= [] andalso RedirectUrl /= undefined,

	% Get the content to send back...
  Content1 =case IsRedirect of 
		true -> wf_handle:build_post_redirect(RedirectUrl);
		false -> Content
	end,

	% Send a message with the content to the comet Pid
	CometPid = wf:state(wf_comet_pid),
	CometPid!{content, Content1},
	
	reset_nitrogen_state().

	
reset_nitrogen_state() ->
	% Clear some state variables...
	L = [wf_action_queue, wf_update_queue, wf_content_script, wf_script, wf_paths, wf_headers],
	[put(X, []) || X <- L].


get_content() ->
	CometPid = wf:state(wf_comet_pid),
	CometPid!{self(), get_content},
	receive
		{content, []} -> 
			timer:sleep(?CONTENT_LOOP_INTERVAL),
			get_content();
		
		{content, Content} -> 
			Content
	after ?INACTIVITY_TIMEOUT -> []
	end.				

%%% COMET LOOP %%%	
	
%% ensure_comet_loop/0 - Start the comet loop if it's not already started.
ensure_comet_loop(Timeout) ->
	CometPid = wf:state(wf_comet_pid),
	IsCometPidAlive = CometPid /= undefined andalso wf_utils:is_process_alive(CometPid),
	case IsCometPidAlive of
		true -> 
			ok;
		false ->
			wf:wire(#comet_start {}),
			Pid = spawn(fun() -> comet_loop(Timeout) end),
			wf:state(wf_comet_pid, Pid)
	end.

			
%% comet_loop/2 - Spawned for each page with a #comet on it.
%% Functions send content here, and it is saved in the mailbox
%% until a get_content request comes along to read it.
%% By default, it stops in 5 seconds.
comet_loop(Timeout) ->
	receive
		{Pid, get_content} -> 
			Content = collect_content(),
			Pid!{content, Content},
			comet_loop(Timeout)
			
	after 
		Timeout -> stop
	end.

%% collect_content/0 - Gather all content sent to this comet_loop.
collect_content() -> lists:reverse(inner_collect_content()).
inner_collect_content() ->
	receive 
		{content, C} -> [C|inner_collect_content()]
	after	
		0 -> [] 
	end.