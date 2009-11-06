% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_event).
-include ("wf.inc").
-export ([
	update_context_with_event/0,
	generate_postback_script/4,
	generate_postback_script/5,
	generate_system_postback_script/4,
	serialize_event_context/4
]).

% This module looks at the incoming request for 'eventContext' and 'pageContext' params. 
% If found, then it updates the current context, putting values into event_context
% and page_context, respectively.
%
% If not found, then it creates #event_context and #page_context records with
% values for a first request.


update_context_with_event() ->
	SerializedEvent = wf:q(eventContext),
	Event = wf_pickle:depickle(event_schema(), SerializedEvent),

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
	wf_context:current_path(["page"]),
	ok.

update_context_for_postback_request(Event) ->
	TriggerPath = wf_path:normalize_path(Event#event_context.trigger),
	TargetPath = wf_path:normalize_path(Event#event_context.target),
	wf_context:type(postback_request),
	wf_context:event_context(Event),
	wf_context:current_path(TargetPath),
	wf_context:event_trigger(TriggerPath),
	wf_context:event_target(TargetPath),
	ok.

generate_postback_script(Postback, TriggerPath, TargetPath, Delegate) -> 
	generate_postback_script(Postback, TriggerPath, TargetPath, Delegate, "''").
	
generate_postback_script(undefined, _TriggerPath, _TargetPath, _Delegate, _ExtraParam) -> [];
generate_postback_script(Postback, TriggerPath, TargetPath, Delegate, ExtraParam) ->
	PickledPostbackInfo = serialize_event_context(Postback, TriggerPath, TargetPath, Delegate),
	[
		wf_render_actions:generate_scope_script(),
		wf:f("Nitrogen.$queue_event('~s', '~s', ~s);", [wf:to_js_id(TriggerPath), PickledPostbackInfo, ExtraParam])
	].

generate_system_postback_script(undefined, _TriggerPath, _TargetPath, _Delegate) -> [];
generate_system_postback_script(Postback, TriggerPath, TargetPath, Delegate) ->
	PickledPostbackInfo = serialize_event_context(Postback, TriggerPath, TargetPath, Delegate),
	[
		wf_render_actions:generate_scope_script(),
		wf:f("Nitrogen.$queue_system_event('~s');", [PickledPostbackInfo])
	].
	
serialize_event_context(Tag, TriggerPath, TargetPath, Delegate) ->
	PageModule = wf_context:page_module(),
	EventModule = wf:coalesce([Delegate, PageModule]),
	Event = #event_context {
		module = EventModule,
		tag = Tag,
		trigger = TriggerPath,
		target = TargetPath
	},
	wf_pickle:pickle(event_schema(), Event).
	
	
event_schema() ->
	#event_context {
		module=atom@,
		tag=term@,
		trigger=term@,
		target=term@
	}.