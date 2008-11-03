-module (wf).
-include ("wf.inc").
-compile (export_all).

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

debug() -> wf_utils:debug().
break() -> wf_utils:break().


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

unsafe_update(Element, Terms) -> wf_render:unsafe_update(Element, Terms).
unsafe_insert_top(Element, Terms) -> wf_render:unsafe_insert_top(Element, Terms).
unsafe_insert_bottom(Element, Terms) -> wf_render:unsafe_insert_bottom(Element, Terms).

wire(Actions) -> wf_render:wire(Actions).
wire(TargetID, Actions) -> wf_render:wire(TargetID, Actions).
wire(TriggerID, TargetID, Actions) -> wf_render:wire(TriggerID, TargetID, Actions).


%%% WF_CONTINUE %%%

continue(Tag, Function) -> wf_continuation:continue(Tag, Function).
continue(Tag, Function, Interval) -> wf_continuation:continue(Tag, Function, Interval).
continue(Tag, Function, Interval, Timeout) -> wf_continuation:continue(Tag, Function, Interval, Timeout).


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


%%% WF_QUERY %%%

q(Q) -> wf_query:q(Q).


%%% WF_PATH %%%

me_var() -> wf_render:me_var().
temp_id() -> wf_path:temp_id().
to_ident(Path) -> wf_path:to_ident(Path).


%%% OTHER %%%

flash(Terms) -> element_flash:add_flash(Terms).

assert(true, _) -> ok;
assert(false, Error) -> erlang:error(Error).





