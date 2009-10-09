% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf).
-include ("wf.inc").
-compile (export_all).

%%% EXPOSE PAGE CONTEXT %%%

page_module() -> wf_context:page_module().

%%% EXPOSE WIRE, UPDATE, FLASH %%%
wire(Actions) -> 
	ok = wire(undefined, undefined, Actions).
	
wire(TargetID, Actions) -> 
	ok = wire(TargetID, TargetID, Actions).
	
wire(TriggerID, TargetID, Actions) -> 
	ok = action_wire:wire(TriggerID, TargetID, Actions).

update(TargetID, Elements) -> 
	ok = action_update:update(TargetID, Elements).
	
replace(TargetID, Elements) ->
	ok = action_update:replace(TargetID, Elements).
	
insert_top(TargetID, Elements) -> 
	ok = action_update:insert_top(TargetID, Elements).
	
insert_bottom(TargetID, Elements) -> 
	ok = action_update:insert_bottom(TargetID, Elements).

flash(Elements) ->
	ok = element_flash:add_flash(Elements).

%%% EXPOSE WF_UTILS %%%

f(S) -> 
	_String = wf_utils:f(S).
	
f(S, Args) -> 
	_String = wf_utils:f(S, Args).
	
coalesce(L) -> 
	_Value = wf_utils:coalesce(L).
	
	
	
%%% WF_REDIRECT %%%
redirect(Url) -> 
	action_redirect:redirect(Url).

% redirect_to_login(Url) -> wf_redirect:redirect_to_login(Url).
% redirect_from_login(DefaultUrl) -> wf_redirect:redirect_from_login(DefaultUrl).



%%% EXPOSE WF_PICKLE %%%
pickle(Data) -> 
	_SerializedData = wf_pickle:pickle(Data).
	
depickle(SerializedData) -> 
	_Data = wf_pickle:depickle(SerializedData).



%%% EXPOSE WF_CONVERT %%%
to_list(T) -> 
	_String = wf_convert:to_list(T).
	
to_atom(T) -> 
	_Atom = wf_convert:to_atom(T).
	
to_binary(T) -> 
	_Binary = wf_convert:to_binary(T).
	
to_integer(T) -> 
	_Integer = wf_convert:to_integer(T).
	
clean_lower(S) -> 
	_String = wf_convert:clean_lower(S).
	
html_encode(S) -> 
	_String = wf_convert:html_encode(S).
	
html_encode(S, Encode) -> 
	_String = wf_convert:html_encode(S, Encode).


%%% EXPOSE WF_BIND %%%
% TODO
set(Element, Value) -> 
	ok = action_set:set(Element, Value).
	
% bind(BindingTuple, Record) -> wf_bind:bind(BindingTuple, Record).
% reverse_bind(BindingTuple) -> wf_bind:reverse_bind(BindingTuple).
% reverse_bind(BindingTuple, Record) -> wf_bind:reverse_bind(BindingTuple, Record).



%%% OTHER %%%
% TODO
% logout() -> clear_user(), clear_roles(), clear_state(), clear_session().
to_js_id(Path) -> 
	_String = wf_render_actions:to_js_id(Path).
	
js_escape(String) -> 
	_String = wf_utils:js_escape(String).
	
to_html_id(Path) -> 
	_String = wf_render_elements:to_html_id(Path).
	
temp_id() -> 
	_String = wf_render_elements:temp_id().
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% EXPOSE HEADERS %%%

status_code() -> 
	ok = wf_context:status_code().
	
status_code(StatusCode) ->
	ok = wf_context:status_code(StatusCode).
	
content_type(ContentType) ->
	ok = wf_context:content_type(ContentType).
	
headers() -> 
	wf_context:headers().
	
header(Header) ->
	wf_context:header(Header).

header(Header, Value) ->
	ok = wf_context:header(Header, Value).
	
cookies() ->
	wf_context:cookies().
	
cookie(Cookie) ->
	wf_context:cookie(Cookie).

cookie(Cookie, Value) ->
	ok = wf_context:cookie(Cookie, Value).

cookie(Cookie, Value, Path, MinutesToLive) ->
	ok = wf_context:cookie(Cookie, Value, Path, MinutesToLive).

	
%%% EXPOSE QUERY_HANDLER %%%
q(Key) -> 
	_String = query_handler:get_value(Key).



%%% EXPOSE LOG_HANDLER %%%
info(String, Args) -> 
	ok = log_handler:info(String, Args).
	
info(String) -> 
	ok = log_handler:info(String).

warning(String, Args) -> 
	ok = log_handler:warning(String, Args).

warning(String) -> 
	ok = log_handler:warning(String).

error(String, Args) -> 
	ok = log_handler:error(String, Args).

error(String) -> 
	ok = log_handler:error(String).



%%% EXPOSE SESSION_HANDLER %%% 
session(Key) -> 
	_Value = session_handler:get_value(Key).

session(Key, Value) -> 
	ok = session_handler:set_value(Key, Value).

clear_session(Key) ->
	ok = session_handler:clear(Key).

clear_session() -> 
	ok = session_handler:clear_all().



%%% EXPOSE IDENTITY_HANDLER %%%
user() -> 
	_User = identity_handler:get_user().	

user(User) -> 
	ok = identity_handler:set_user(User).

clear_user() -> 
	ok = identity_handler:clear().



%%% EXPOSE ROLE_HANDLER %%%
role(Role) -> 
	_Boolean = role_handler:get_has_role(Role).

role(Role, IsInRole) -> 
	ok = role_handler:set_has_role(Role, IsInRole).

roles() ->
	_Roles = role_handler:roles().

clear_roles() -> 
	ok = role_handler:clear_all().



%%% EXPOSE STATE_HANDLER %%%
state(Key) -> 
	_Value = state_handler:get_state(Key).

state(Key, Value) -> 
	ok = state_handler:set_state(Key, Value).

clear_state(Key) ->
	ok = state_handler:clear(Key).
	
clear_state() -> 
	ok = state_handler:clear_all().



%%% EXPOSE ACTION_ASYNC %%%
send(Pool, Message) ->
	ok = action_async:send(Pool, Message).

send_global(Pool, Message) ->
	ok = action_async:send_global(Pool, Message).

flush() ->
	ok = action_async:flush().

get_async_mode() -> wf_context:async_mode().
set_async_mode(AsyncMode) -> wf_context:async_mode(AsyncMode).
switch_to_comet() -> set_async_mode(comet).
switch_to_polling(IntervalInMS) -> set_async_mode({poll, IntervalInMS}).

%%% DEBUGGING %%%
debug() -> wf_utils:debug().
break() -> wf_utils:break().
assert(true, _) -> ok;
assert(false, Error) -> erlang:error(Error).
