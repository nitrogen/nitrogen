% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_comet).
-include ("wf.inc").
-export ([
	comet/1,
	comet_flush/0,
	get_content/0
]).

% Comet in Nitrogen has three moving parts:
%
% 1. A comet function (passed into comet/1) that will generate content 
%    and send it to a middleman process. A page can have multiple comet 
%    functions running at the same time.
%
% 2. A middleman process that buffers new content and sends it to the browser.
%    A page only has one middleman process running at any given time. If the middleman
%    doesn't see any action for COMET_REQUEST_TIMEOUT milliseconds, it will assume
%    that the user has navigated away from the page and die, killing the comet function 
%    above in the process.
%    
% 3. The process created when the browser opens a long-running http request. This 
%    process will ask the middleman for data at a rate of CONTENT_LOOP_INTERVAL for up to
%    INACTIVITY_TIMEOUT milliseconds, and deliver the content to the browser in the form
%    of Javascript.

% How long should we wait for a browser to request comet content
% before we say that the user has navigated away from the current
% page? 
-define(COMET_REQUEST_TIMEOUT, 20000).

% How long should we wait (on the server side) between polls when
% polling the comet function for new content?
-define(CONTENT_LOOP_INTERVAL, 300).

% How long should we wait for the comet function to generate content
% before just returning a blank string to the browser. When this timeout
% is reached, the browser will stop the current open comet request
% and start a new one.
-define(INACTIVITY_TIMEOUT, 20000).


comet(Function) -> comet(long_poll, Function).

comet(Type, Function) ->
	% Make sure the comet loop is running...
	ensure_comet_loop(),
	
	CurrentState = get(),
	F = fun() ->
		% Copy state to the new process...
		[put(X, Y) || {X, Y} <- CurrentState],
		reset_nitrogen_state(),		
		
		% HACK - We don't know if the page has flash or not,
		% but assume it does, because if a comet function is
		% present, then 99% of the time it will use flash.
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
ensure_comet_loop() ->
	CometPid = wf:state(wf_comet_pid),
	IsCometPidAlive = CometPid /= undefined andalso wf_utils:is_process_alive(CometPid),
	case IsCometPidAlive of
		true -> 
			ok;
		false ->
			wf:wire(#comet_start {}),
			Pid = spawn(fun() -> comet_loop() end),
			wf:state(wf_comet_pid, Pid)
	end.

			
%% comet_loop/2 - Spawned for each page with a #comet on it.
%% Functions send content here, and it is saved in the mailbox
%% until a get_content request comes along to read it.
%% If we do not get a get_content request after the Timeout, then
%% assume the user has left.
comet_loop() ->
	receive
		{Pid, get_content} -> 
			Content = collect_content(),
			Pid!{content, Content},
			comet_loop()
			
	after ?COMET_REQUEST_TIMEOUT -> 
			% Haven't seen any action in a while. Assume that the user
			% has left the page. Exit to kill any comet functions running
			% on the page.
			exit(stop_comet)
	end.

%% collect_content/0 - Gather all content sent to this comet_loop.
collect_content() -> lists:reverse(inner_collect_content()).
inner_collect_content() ->
	receive 
		{content, C} -> [C|inner_collect_content()]
	after	
		0 -> [] 
	end.