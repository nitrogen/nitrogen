% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (render_handler).
-include ("wf.inc").
-export ([
	behaviour_info/1,
	render/3,
	wire/4,
	update/3,
	insert_top/3,
	insert_bottom/3
]).

behaviour_info(callbacks) -> [
	% init(Context) -> {ok, NewContext, State}.
	% Called at the start of the request.
	{init, 1},      

	% finish(Context, State) -> {ok, NewContext, NewState}.
	% Called at the end of the request, before sending
	% a response back to the browser.
	{finish, 2},

	% render_postback(Elements, Actions, Context, State) -> {ok, Html, Script, NewContext, NewState}.
	% Render elements and actions into HTML and Javascript.
	{render, 4}
];

behaviour_info(_) -> undefined.

render(Elements, Actions, Context) ->
	wf_context:apply(render, render, [Elements, Actions], Context).

wire(TriggerID, TargetID, Actions, Context) ->
	CurrentPath = Context#context.current_path,
	WireAction = #wire {
		trigger=wff:coalesce([TriggerID, CurrentPath]),
		target=wff:coalesce([TargetID, CurrentPath]),
		actions=Actions
	},
	queue_action(WireAction, Context).

update(TargetID, Elements, Context) -> 
	update(update, TargetID, Elements, Context).

insert_top(TargetID, Elements, Context) -> 
	update(insert_top, TargetID, Elements, Context).

insert_bottom(TargetID, Elements, Context) -> 
	update(insert_bottom, TargetID, Elements, Context).


%%% PRIVATE FUNCTIONS %%%

update(Type, TargetID, Elements, Context) ->
	UpdateAction = #update {
		type=Type,
		target=TargetID,
		elements=Elements		
	},
	queue_action(UpdateAction, Context).
	
queue_action(Action, Context) ->
	{ok, Context#context {
		queued_actions = [Action|Context#context.queued_actions]
	}}.

	