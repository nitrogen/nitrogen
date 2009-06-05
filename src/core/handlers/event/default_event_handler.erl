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
	% Decode the eventInfo...
	PostbackInfo = wff:q(postbackInfo, Context),
	Result = wff:depickle(PostbackInfo, Context),
	Context1 = case Result of 
		{ObjectID, Tag, EventType, TriggerID, TargetID, Delegate} ->
			% We were able to find and parse postbackInfo. Update the context.
			Context#context {
				is_first_request = false,
				name = ObjectID,
				event_module = Delegate,
				event_type = EventType,
				event_tag = Tag,
				event_trigger = TriggerID,
				event_target = TargetID,
				current_path = ["page"]
			};
			
		_ -> 
			Context#context {
				is_first_request = true,
				name = page,
				event_module = Context#context.page_module,
				current_path = ["page"]
			}
	end,	
	{ok, Context1, []}.
	
finish(Context, State) -> 
	{ok, Context, State}.
	
