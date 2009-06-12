% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wff).
-include ("wf.inc").
-include ("simplebridge.hrl").
-compile (export_all).

% wff.erl is the functional version of wf.erl. Almost every function
% takes a Context argument, and almost every function returns {ok, ..., Context}.
% Using wff is a proper but tedious way of writing applications in Nitrogen.

%%% EXPOSE WIRE, UPDATE, FLASH %%%
wire(Actions, Context) -> 
	{ok, _NewContext} = wire(undefined, undefined, Actions, Context).
	
wire(TargetID, Actions, Context) -> 
	{ok, _NewContext} = wire(TargetID, TargetID, Actions, Context).
	
wire(TriggerID, TargetID, Actions, Context) -> 
	{ok, _NewContext} = action_wire:wire(TriggerID, TargetID, Actions, Context).

update(TargetID, Elements, Context) -> 
	{ok, _NewContext} = action_update:update(TargetID, Elements, Context).
	
insert_top(TargetID, Elements, Context) -> 
	{ok, _NewContext} = action_update:insert_top(TargetID, Elements, Context).
	
insert_bottom(TargetID, Elements, Context) -> 
	{ok, _NewContext} = action_update:insert_bottom(TargetID, Elements, Context).

% flash(Terms) -> element_flash:add_flash(Terms).



%%% EXPOSE WF_UTILS %%%
f(S) -> 
	_String = wf_utils:f(S).
	
f(S, Args) -> 
	_String = wf_utils:f(S, Args).
	
coalesce(L) -> 
	_Value = wf_utils:coalesce(L).
	
	
	
%%% WF_REDIRECT %%%
redirect(Url, Context) -> 
	{ok, _NewContext} = action_redirect:redirect(Url, Context).

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



%%% EXPOSE PAGE CONTEXT %%%
get_path_info(Context) -> 
	Page = Context#context.page_context,
	Page#page_context.path_info.

get_page_module(Context) -> 
	Page = Context#context.page_context,
	Page#page_context.module.



%%% EXPOSE WF_BIND %%%
% TODO
% set(Element, Value) -> wf_bind:set(Element, Value).
% bind(BindingTuple, Record) -> wf_bind:bind(BindingTuple, Record).
% reverse_bind(BindingTuple) -> wf_bind:reverse_bind(BindingTuple).
% reverse_bind(BindingTuple, Record) -> wf_bind:reverse_bind(BindingTuple, Record).



%%% OTHER %%%
% TODO
% logout(Context) -> clear_user(), clear_roles(), clear_state(), clear_session().
to_js_id(Path) -> 
	_String = wf_render_actions:to_js_id(Path).
	
to_html_id(Path) -> 
	_String = wf_render_elements:to_html_id(Path).
	
temp_id() -> 
	_String = wf_render_elements:temp_id().
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% EXPOSE COOKIE HANDLER %%%
cookie(Key, Context) -> 
	_Value = cookie_handler:get_cookie(Key, Context).
	
cookie(Key, Value, Context) -> 
	{ok, _NewContext} = cookie(Key, Value, "/", 20, Context).
	
cookie(Key, Value, Path, MinutesToLive, Context) -> 
	{ok, _NewContext} = cookie_handler:set_cookie(Key, Value, Path, MinutesToLive, Context).


	
%%% EXPOSE QUERY_HANDLER %%%
q(Key, Context) -> 
	_String = query_handler:get_value(Key, Context).



%%% EXPOSE LOG_HANDLER %%%
info(String, Args, Context) -> 
	{ok, _NewContext} = log_handler:info(String, Args, Context).
	
info(String, Context) -> 
	{ok, _NewContext} = log_handler:info(String, Context).

warning(String, Args, Context) -> 
	{ok, _NewContext} = log_handler:warning(String, Args, Context).

warning(String, Context) -> 
	{ok, _NewContext} = log_handler:warning(String, Context).

error(String, Args, Context) -> 
	{ok, _NewContext} = log_handler:error(String, Args, Context).

error(String, Context) -> 
	{ok, _NewContext} = log_handler:error(String, Context).



%%% EXPOSE SESSION_HANDLER %%% 
session(Key, Context) -> 
	{ok, _NewContext} = session_handler:get_value(Key, Context).

session(Key, Value, Context) -> 
	{ok, _NewContext} = session_handler:set_value(Key, Value, Context).

clear_session(Key, Context) ->
	{ok, _NewContext} = session_handler:clear(Key, Context).

clear_session(Context) -> 
	{ok, _NewContext} = session_handler:clear_all(Context).



%%% EXPOSE IDENTITY_HANDLER %%%
user(Context) -> 
	_User = identity_handler:get_user(Context).	

user(User, Context) -> 
	{ok, _NewContext} = identity_handler:set_user(User, Context).

clear_user(Context) -> 
	{ok, _NewContext} = identity_handler:clear(Context).



%%% EXPOSE ROLE_HANDLER %%%
role(Role, Context) -> 
	_Boolean = role_handler:get_has_role(Role, Context).

role(Role, IsInRole, Context) -> 
	{ok, _NewContext} = role_handler:set_has_role(Role, IsInRole, Context).

roles(Context) ->
	_Roles = role_handler:roles(Context).

clear_roles(Context) -> 
	{ok, _NewContext} = role_handler:clear_all(Context).



%%% EXPOSE STATE_HANDLER %%%
state(Key, Context) -> 
	_Value = state_handler:get_state(Key, Context).

state(Key, Value, Context) -> 
	{ok, _NewContext} = state_handler:set_state(Key, Value, Context).

clear_state(Key, Context) ->
	{ok, _NewContext} = state_handler:clear(Key, Context).
	
clear_state(Context) -> 
	{ok, _NewContext} = state_handler:clear_all(Context).



%%% EXPOSE ACTION_ASYNC %%%
send(Pool, Message, Context) ->
	{ok, _NewContext} = action_async:send(Pool, Message, Context).

send_global(Pool, Message, Context) ->
	{ok, _NewContext} = action_async:send_global(Pool, Message, Context).

flush(Context) ->
	{ok, _NewContext} = action_async:flush(Context).

switch_to_comet(Context) ->
	Page = Context#context.page_context,
	Context1 = Context#context { 
		%page_context=Page#page_context { async_mode={poll, 1000} }
		page_context=Page#page_context { async_mode=comet }
	},
	{ok, Context1}.
	
switch_to_polling(IntervalInMS, Context) ->
	Page = Context#context.page_context,
	Context1 = Context#context { 
		page_context=Page#page_context { async_mode={poll, IntervalInMS} }
	},
	{ok, Context1}.


%%% DEBUGGING %%%
debug() -> wf_utils:debug().
break() -> wf_utils:break().
assert(true, _) -> ok;
assert(false, Error) -> erlang:error(Error).
