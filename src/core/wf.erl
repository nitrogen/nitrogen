% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf).
-include ("wf.inc").
-include ("simplebridge.hrl").
-compile (export_all).

% wff.erl is the non-functional version of wf.erl. Almost every function
% pulls Context out of the process dictionary, does stuff on it, and puts
% it back into the process dictionary.
% Using wf is the improper but fun way of writing applications in Nitrogen.


% Each statement that changes a context is wrapped with ?PRE and ?POST.
% Thes take care of getting and putting the Context.
-define (PRE, Context = get(context)).
-define (POST, put(context, NewContext)).

%%% EXPOSE WIRE, UPDATE, FLASH %%%
wire(Actions) -> 
	?PRE, {ok, NewContext} = wff:wire(Actions, Context), ?POST, ok.
	
wire(TargetID, Actions) -> 
	?PRE, {ok, NewContext} = wff:wire(TargetID, TargetID, Actions, Context), ?POST, ok.
	
wire(TriggerID, TargetID, Actions) -> 
	?PRE, {ok, NewContext} = wff:wire(TriggerID, TargetID, Actions, Context), ?POST, ok.

update(TargetID, Elements) -> 
	?PRE, {ok, NewContext} = wff:update(TargetID, Elements, Context), ?POST, ok.
	
insert_top(TargetID, Elements) -> 
	?PRE, {ok, NewContext} = wff:insert_top(TargetID, Elements, Context), ?POST, ok.
	
insert_bottom(TargetID, Elements) -> 
	?PRE, {ok, NewContext} = wff:insert_bottom(TargetID, Elements, Context), ?POST, ok.

% flash(Terms) -> element_flash:add_flash(Terms).



%%% EXPOSE WF_UTILS %%%
f(S) -> 
	_String = wf_utils:f(S).
	
f(S, Args) -> 
	_String = wf_utils:f(S, Args).
	
coalesce(L) -> 
	_Value = wf_utils:coalesce(L).
	
	
	
%%% WF_REDIRECT %%%
redirect(Url) -> 
	?PRE, {ok, NewContext} = wff:redirect(Url, Context), ?POST, ok.

% redirect_to_login(Url) -> wf_redirect:redirect_to_login(Url).
% redirect_from_login(DefaultUrl) -> wf_redirect:redirect_from_login(DefaultUrl).



%%% EXPOSE WF_PICKLE %%%
pickle(Data) -> 
	_SerializedData = wff:pickle(Data).
	
depickle(SerializedData) -> 
	_Data = wff:depickle(SerializedData).



%%% EXPOSE WF_CONVERT %%%
to_list(T) -> 
	_String = wff:to_list(T).
	
to_atom(T) -> 
	_Atom = wff:to_atom(T).
	
to_binary(T) -> 
	_Binary = wff:to_binary(T).
	
to_integer(T) -> 
	_Integer = wff:to_integer(T).
	
clean_lower(S) -> 
	_String = wff:clean_lower(S).
	
html_encode(S) -> 
	_String = wff:html_encode(S).
	
html_encode(S, Encode) -> 
	_String = wff:html_encode(S, Encode).



%%% EXPOSE PAGE CONTEXT %%%
get_path_info() -> 
	?PRE, _String = wff:get_path_info(Context).

get_page_module() -> 
	?PRE, _String = wff:get_page_module(Context).



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
	_String = wff:to_js_id(Path).
	
to_html_id(Path) -> 
	_String = wff:to_html_id(Path).
	
temp_id() -> 
	_String = wff:temp_id().
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% EXPOSE COOKIE HANDLER %%%
get_cookie(Key) -> 
	?PRE, _Value = wff:get_cookie(Key, Context).
	
set_cookie(Key, Value) -> 
	?PRE, {ok, NewContext} = wff:set_cookie(Key, Value, Context), ?POST, ok.
	
set_cookie(Key, Value, Path, MinutesToLive) -> 
	?PRE, {ok, NewContext} = wff:set_cookie(Key, Value, Path, MinutesToLive, Context), ?POST, ok.


	
%%% EXPOSE QUERY_HANDLER %%%
q(Key) -> 
	?PRE, _String = wff:q(Key, Context).



%%% EXPOSE LOG_HANDLER %%%
info(String, Args) -> 
	?PRE, {ok, NewContext} = wff:info(String, Args, Context), ?POST, ok.
	
info(String) -> 
	?PRE, {ok, NewContext} = wff:info(String, Context), ?POST, ok.

warning(String, Args) -> 
	?PRE, {ok, NewContext} = wff:warning(String, Args, Context), ?POST, ok.

warning(String) -> 
	?PRE, {ok, NewContext} = wff:warning(String, Context), ?POST, ok.

error(String, Args) -> 
	?PRE, {ok, NewContext} = wff:error(String, Args, Context), ?POST, ok.

error(String) -> 
	?PRE, {ok, NewContext} = wff:error(String, Context), ?POST, ok.



%%% EXPOSE SESSION_HANDLER %%% 
session(Key) -> 
	?PRE, {ok, NewContext} = wff:session(Key, Context), ?POST, ok.

session(Key, Value) -> 
	?PRE, {ok, NewContext} = wff:session(Key, Value, Context), ?POST, ok.

clear_session(Key) ->
	?PRE, {ok, NewContext} = wff:clear_session(Key, Context), ?POST, ok.

clear_session() -> 
	?PRE, {ok, NewContext} = wff:clear_session(Context), ?POST, ok.



%%% EXPOSE IDENTITY_HANDLER %%%
user() -> 
	?PRE, _User = wff:user(Context).	

user(User) -> 
	?PRE, {ok, NewContext} = wff:user(User, Context), ?POST, ok.

clear_user(Context) -> 
	?PRE, {ok, NewContext} = wff:clear_user(Context), ?POST, ok.



%%% EXPOSE ROLE_HANDLER %%%
role(Role) -> 
	?PRE, _Boolean = wff:role(Role, Context).

role(Role, IsInRole) -> 
	?PRE, {ok, NewContext} = wff:role(Role, IsInRole, Context), ?POST, ok.

roles() ->
	?PRE, _Roles = wff:roles(Context).

clear_roles() -> 
	?PRE, {ok, NewContext} = wff:clear_roles(Context), ?POST, ok.



%%% EXPOSE STATE_HANDLER %%%
state(Key) -> 
	?PRE, _Value = wff:state(Key, Context).

state(Key, Value) -> 
	?PRE, {ok, NewContext} = wff:state(Key, Value, Context), ?POST, ok.

clear_state(Key) ->
	?PRE, {ok, NewContext} = wff:clear_state(Key, Context), ?POST, ok.
	
clear_state() -> 
	?PRE, {ok, NewContext} = wff:clear_state(Context), ?POST, ok.



%%% EXPOSE ACTION_ASYNC %%%
send(Pool, Message) ->
	?PRE, {ok, NewContext} = wff:send(Pool, Message, Context), ?POST, ok.

send_global(Pool, Message) ->
	?PRE, {ok, NewContext} = wff:send_global(Pool, Message, Context), ?POST, ok.

flush() ->
	?PRE, {ok, NewContext} = wff:flush(Context), ?POST, ok.

switch_to_comet() ->
	?PRE, {ok, NewContext} = wff:switch_to_comet(Context), ?POST, ok.

switch_to_polling(IntervalInMS) ->
	?PRE, {ok, NewContext} = wff:switch_to_polling(IntervalInMS, Context), ?POST, ok.



%%% DEBUGGING %%%
debug() -> wff:debug().
break() -> wff:break().
assert(Result, ErrorMessage) -> wff:assert(Result, ErrorMessage).
