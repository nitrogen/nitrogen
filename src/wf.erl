% Copyright (c) 2008-2009 Rusty Klophaus
% 
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
% 
% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.
% 
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
% THE SOFTWARE.

-module (wf).
-include ("wf.inc").
-compile (export_all).

%%% EXPOSE WF_INIT %%%

init() -> wf_init:init().

%%% EXPOSE WF_UTILS %%%

path_to_module(Path) -> wf_utils:path_to_module(Path).

f(S) -> wf_utils:f(S).
f(S, Args) -> wf_utils:f(S, Args).
coalesce(L) -> wf_utils:coalesce(L).

id() -> wf_utils:id().
guid() -> wf_utils:guid().

pickle(Data) -> wf_utils:pickle(Data).
depickle(Data) -> wf_utils:depickle(Data).
depickle(Data, SecondsToLive) -> wf_utils:depickle(Data, SecondsToLive).

is_string(Term) -> wf_utils:is_string(Term).

debug() -> wf_utils:debug().
break() -> wf_utils:break().

%%% EXPOSE WF_PLATFORM %%%
get_path_info() -> wf_platform:get_path_info().
get_page_module() -> wf_platform:get_page_module().
set_cookie(Key, Value) -> wf_platform:set_cookie(Key, Value).
set_cookie(Key, Value, Path, MinutesToLive) -> wf_platform:set_cookie(Key, Value, Path, MinutesToLive).
get_cookie(Key) -> wf_platform:get_cookie(Key).
set_response_code(Code) -> wf_platform:set_response_code(Code).
set_content_type(ContentType) -> wf_platform:set_content_type(ContentType).



%%% EXPOSE WF_BIND %%%

set(Element, Value) -> wf_bind:set(Element, Value).
bind(BindingTuple, Record) -> wf_bind:bind(BindingTuple, Record).
reverse_bind(BindingTuple) -> wf_bind:reverse_bind(BindingTuple).
reverse_bind(BindingTuple, Record) -> wf_bind:reverse_bind(BindingTuple, Record).



%%% EXPOSE WF_CONVERT %%%

to_list(T) -> wf_convert:to_list(T).
to_atom(T) -> wf_convert:to_atom(T).
to_binary(T) -> wf_convert:to_binary(T).
to_integer(T) -> wf_convert:to_integer(T).
clean_lower(S) -> wf_convert:clean_lower(S).
html_encode(S) -> wf_convert:html_encode(S).
html_encode(S, Encode) -> wf_convert:html_encode(S, Encode).


%%% EXPOSE WF_RENDER %%%

render(Terms) -> wf_render:render(Terms).

update(Element, Terms) -> wf_render:update(Element, Terms).
insert_top(Element, Terms) -> wf_render:insert_top(Element, Terms).
insert_bottom(Element, Terms) -> wf_render:insert_bottom(Element, Terms).

wire(Actions) -> wf_render:wire(Actions).
wire(TargetID, Actions) -> wf_render:wire(TargetID, Actions).
wire(TriggerID, TargetID, Actions) -> wf_render:wire(TriggerID, TargetID, Actions).


%%% WF_CONTINUE %%%

continue(Tag, Function) -> wf_continuation:continue(Tag, Function).
continue(Tag, Function, Interval) -> wf_continuation:continue(Tag, Function, Interval).
continue(Tag, Function, Interval, Timeout) -> wf_continuation:continue(Tag, Function, Interval, Timeout).


%%% WF_COMET %%%

comet(Function) -> wf_comet:comet(Function).
comet_flush() -> wf_comet:comet_flush().


%%% WF_REDIRECT %%%

redirect(Url) -> wf_redirect:redirect(Url).
redirect_to_login(Url) -> wf_redirect:redirect_to_login(Url).
redirect_from_login(DefaultUrl) -> wf_redirect:redirect_from_login(DefaultUrl).



%%% WF_SESSION %%%

state(Key) -> wf_state:state(Key).
state(Key, Value) -> wf_state:state(Key, Value).
clear_state() -> wf_state:clear_state().


%%% WF_STATE %%%

user() -> wf_session:user().
user(User) -> wf_session:user(User).
clear_user() -> wf_session:clear_user().

role(Role) -> wf_session:role(Role).
role(Role, IsInRole) -> wf_session:role(Role, IsInRole).
clear_roles() -> wf_session:clear_roles().

session(Key) -> wf_session:session(Key).
session(Key, Value) -> wf_session:session(Key, Value).
clear_session() -> wf_session:clear_session().

logout() -> clear_user(), clear_roles(), clear_state(), clear_session().


%%% WF_CACHE %%%

cache(Key, Function) -> wf_cache:cache(Key, Function).
cache(Key, Function, Options) -> wf_cache:cache(Key, Function, Options).


%%% WF_QUERY %%%

q(Q) -> wf_query:q(Q).


%%% WF_PATH %%%

me_var() -> wf_render:me_var().
temp_id() -> wf_path:temp_id().
to_js_id(Path) -> wf_path:to_js_id(Path).


%%% OTHER %%%

flash(Terms) -> element_flash:add_flash(Terms).

assert(true, _) -> ok;
assert(false, Error) -> erlang:error(Error).





