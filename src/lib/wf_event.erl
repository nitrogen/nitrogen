% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_event).
-include ("wf.inc").
-export ([
	update_context_with_event/0,
	generate_postback_script/6,
	generate_system_postback_script/5,
	serialize_event_context/5,
	generate_anchor_script/1
]).

% This module looks at the incoming request for 'eventContext' and 'pageContext' params. 
% If found, then it updates the current context, putting values into event_context
% and page_context, respectively.
%
% If not found, then it creates #event_context and #page_context records with
% values for a first request.


update_context_with_event() ->
	SerializedEvent = wf:q(eventContext),
	Event = wf_pickle:depickle(SerializedEvent),

	% Update the Context...
	PageModule = wf_context:page_module(),
	IsPostback = is_record(Event, event_context),
	case {PageModule, IsPostback} of
		{static_file, _} -> update_context_for_static_file();
		{_, false}       -> update_context_for_first_request();
		{_, true}        -> update_context_for_postback_request(Event)
	end.
	
update_context_for_static_file() ->
	wf_context:type(static_file),
	ok.
	
update_context_for_first_request() ->
	Module = wf_context:page_module(),
	wf_context:event_module(Module),
	wf_context:type(first_request),
	wf_context:anchor("page"),
	ok.

update_context_for_postback_request(Event) ->
	Anchor = Event#event_context.anchor,
	Trigger = Event#event_context.trigger,
	Target = Event#event_context.target,
	wf_context:type(postback_request),
	wf_context:event_context(Event),
	wf_context:anchor(Anchor),
	wf_context:event_trigger(Trigger),
	wf_context:event_target(Target),
	ok.

generate_postback_script(undefined, _Anchor, _Trigger, _Target, _Delegate, _ExtraParam) -> [];
generate_postback_script(Postback, Anchor, Trigger, Target, Delegate, ExtraParam) ->
	PickledPostbackInfo = serialize_event_context(Postback, Anchor, Trigger, Target, Delegate),
	wf:f("Nitrogen.$queue_event('~s', '~s', '~s', ~s);", [Anchor, Trigger, PickledPostbackInfo, ExtraParam]).

generate_system_postback_script(undefined, _Anchor, _Trigger, _Target, _Delegate) -> [];
generate_system_postback_script(Postback, Anchor, Trigger, Target, Delegate) ->
	PickledPostbackInfo = serialize_event_context(Postback, Anchor, Trigger, Target, Delegate),
	wf:f("Nitrogen.$queue_system_event('~s');", [PickledPostbackInfo]).
	
serialize_event_context(Tag, Anchor, Trigger, Target, Delegate) ->
	PageModule = wf_context:page_module(),
	EventModule = wf:coalesce([Delegate, PageModule]),
	Event = #event_context {
		module = EventModule,
		tag = Tag,
		anchor = Anchor,
		trigger = Trigger,
		target = Target
	},
	wf_pickle:pickle(Event).
	
generate_anchor_script(Anchor) ->
	wf:f("Nitrogen.$anchor('~s');", [Anchor]).