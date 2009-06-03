% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_event_handler).
-behaviour (event_handler).
-include ("wf.inc").
-export ([
	init/1, 
	finish/2
]).

init(Context) -> 
	% Get and decode the eventInfo...
	{ok, PostbackInfo, Context1} = wff:q(postbackInfo, Context),
	?PRINT(PostbackInfo),
	{ok, Result, Context2}  = wff:depickle(PostbackInfo, Context1),
	?PRINT(Result),
	Context3 = case Result of 
		{ObjectID, Tag, EventType, TriggerID, TargetID, Delegate} ->
			% We were able to find and parse postbackInfo. Update the context.
			Context2#context {
				is_first_request = false,
				name = ObjectID,
				event_module = Delegate,
				event_type = EventType,
				event_tag = Tag,
				event_trigger = TriggerID,
				event_target = TargetID
			};
			
		_ -> 
			Context2#context {
				is_first_request = true,
				name = page,
				event_module = Context#context.page_module
			}
	end,
	{ok, Context3, []}.
	
finish(Context, State) -> 
	{ok, Context, State}.
	
