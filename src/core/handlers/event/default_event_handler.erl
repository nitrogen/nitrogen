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
	PageModule = Context#context.page_module,
	NewContext = Context#context {
		is_first_request = true,
		event_module = PageModule,
		event_type = undefined,
		event_tag = undefined,
		event_trigger = page,
		event_target = page
	},

	{ok, NewContext, []}.
	
finish(Context, State) -> 
	{ok, Context, State}.
	
