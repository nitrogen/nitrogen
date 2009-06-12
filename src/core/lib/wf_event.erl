% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_event).
-include ("wf.inc").
-export ([
	update_context_with_event/1,
	generate_postback_script/6,
	generate_system_postback_script/6,
	serialize_event_context/6
]).

% This module looks at the incoming requets for 'eventContext' and 'pageContext' params. 
% If found, then it updates the current context, putting values into event_context
% and page_context, respectively.
%
% If not found, then it creates #event_context and #page_context records with
% values for a first request.

update_context_with_event(Context) ->
	% Decode the event_context...
	SerializedEvent = wff:q(eventContext, Context),
	Event = wff:depickle(SerializedEvent),
	
	% Update the Context...
	IsPostback = is_record(Event, event_context),
	case IsPostback of
		false -> update_context_for_first_request(Context);
		true  -> update_context_for_postback_request(Event, Context)
	end.
	
update_context_for_first_request(Context) ->
	Page = Context#context.page_context,
	Module = Page#page_context.module,	
	Event = Context#context.event_context,
	Event1 = Event#event_context {
		module=Module,
		is_first_request = true
	},
	Context1 = Context#context { 
		event_context = Event1,
		current_path=["page"]
	},
	{ok, Context1}.

update_context_for_postback_request(Event, Context) ->
	Context1 = Context#context { 
		event_context = Event,
		current_path = Event#event_context.target
	},
	{ok, Context1}.

generate_postback_script(undefined, _EventType, _TriggerPath, _TargetPath, _Delegate, _Context) -> [];
generate_postback_script(Postback, EventType, TriggerPath, TargetPath, Delegate, Context) ->
	PickledPostbackInfo = serialize_event_context(Postback, EventType, TriggerPath, TargetPath, Delegate, Context),
	[
		wf_render_actions:generate_scope_script(Context),
		wff:f("Nitrogen.$queue_event('~s', '~s');", [wff:to_js_id(TriggerPath), PickledPostbackInfo])
	].

generate_system_postback_script(undefined, _EventType, _TriggerPath, _TargetPath, _Delegate, _Context) -> [];
generate_system_postback_script(Postback, EventType, TriggerPath, TargetPath, Delegate, Context) ->
	PickledPostbackInfo = serialize_event_context(Postback, EventType, TriggerPath, TargetPath, Delegate, Context),
	[
		wf_render_actions:generate_scope_script(Context),
		wff:f("Nitrogen.$do_system_event('~s');", [PickledPostbackInfo])
	].
	
serialize_event_context(Tag, EventType, TriggerPath, TargetPath, Delegate, Context) ->
	Page = Context#context.page_context,
	Module = wff:coalesce([Delegate, Page#page_context.module]),
	Event = #event_context {
		is_first_request = false,
		module = Module,
		tag = Tag,
		type = EventType,
		trigger = TriggerPath,
		target = TargetPath
	},
	wff:pickle(Event).