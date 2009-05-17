% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf).
-include ("wf.inc").
-compile (export_all).

% wf is the backwards compatible approach to Nitrogen programming.
% it redirects all methods to wff.

%%% EXPOSE WF_UTILS %%%

f(S) -> wff:f(S).
f(S, Args) -> wff:f(S, Args).
coalesce(L) -> wff:coalesce(L).

pickle(Data) -> wff:pickle(Data).
depickle(Data) -> wff:depickle(Data).
depickle(Data, SecondsToLive) -> wff:depickle(Data, SecondsToLive).

is_string(Term) -> wff:is_string(Term).

debug() -> wff:debug().
break() -> wff:break().



%%% EXPOSE WF_CONVERT %%%

to_list(T) -> wff:to_list(T).
to_atom(T) -> wff:to_atom(T).
to_binary(T) -> wff:to_binary(T).
to_integer(T) -> wff:to_integer(T).
clean_lower(S) -> wff:clean_lower(S).
html_encode(S) -> wff:html_encode(S).
html_encode(S, Encode) -> wff:html_encode(S, Encode).



%%% EXPOSE WF_PLATFORM %%%

get_path_info() -> wff:get_path_info().
get_page_module() -> wff:get_page_module().
set_cookie(Key, Value) -> wff:set_cookie(Key, Value).
set_cookie(Key, Value, Path, MinutesToLive) -> wff:set_cookie(Key, Value, Path, MinutesToLive).
get_cookie(Key) -> wff:get_cookie(Key).
set_response_code(Code) -> wff:set_response_code(Code).
set_content_type(ContentType) -> wff:set_content_type(ContentType).
get_headers() -> wff:get_headers().
get_header(Hdr) -> wff:get_header(Hdr).



%%% EXPOSE WF_BIND %%%

set(Element, Value) -> wff:set(Element, Value).
bind(BindingTuple, Record) -> wff:bind(BindingTuple, Record).
reverse_bind(BindingTuple) -> wff:reverse_bind(BindingTuple).
reverse_bind(BindingTuple, Record) -> wff:reverse_bind(BindingTuple, Record).


%%% EXPOSE WF_RENDER %%%

render(Terms) -> wff:render(Terms).

update(Element, Terms) -> wff:update(Element, Terms).
insert_top(Element, Terms) -> wff:insert_top(Element, Terms).
insert_bottom(Element, Terms) -> wff:insert_bottom(Element, Terms).

wire(Actions) -> wff:wire(Actions).
wire(TargetID, Actions) -> wff:wire(TargetID, Actions).
wire(TriggerID, TargetID, Actions) -> wff:wire(TriggerID, TargetID, Actions).


%%% WF_CONTINUE %%%

continue(Tag, Function) -> wff:continue(Tag, Function).
continue(Tag, Function, Interval) -> wff:continue(Tag, Function, Interval).
continue(Tag, Function, Interval, Timeout) -> wff:continue(Tag, Function, Interval, Timeout).


%%% WF_COMET %%%

comet(Function) -> wff:comet(Function).
comet_flush() -> wff:comet_flush().


%%% WF_REDIRECT %%%

redirect(Url) -> wff:redirect(Url).
redirect_to_login(Url) -> wff:redirect_to_login(Url).
redirect_from_login(DefaultUrl) -> wff:redirect_from_login(DefaultUrl).



%%% WF_SESSION %%%

state(Key) -> wff:state(Key).
state(Key, Value) -> wff:state(Key, Value).
clear_state() -> wff:clear_state().


%%% WF_STATE %%%

user() -> wff:user().
user(User) -> wff:user(User).
clear_user() -> wff:clear_user().

role(Role) -> wff:role(Role).
role(Role, IsInRole) -> wff:role(Role, IsInRole).
clear_roles() -> wff:clear_roles().

session(Key) -> wff:session(Key).
session(Key, Value) -> wff:session(Key, Value).
clear_session() -> wff:clear_session().

logout() -> clear_user(), clear_roles(), clear_state(), clear_session().


%%% WF_QUERY %%%

q(Q) -> wff:q(Q).


%%% WF_PATH %%%

me_var() -> wff:me_var().
temp_id() -> wff:temp_id().
to_js_id(Path) -> wff:to_js_id(Path).


%%% OTHER %%%

flash(Terms) -> element_flash:add_flash(Terms).

assert(true, _) -> ok;
assert(false, Error) -> erlang:error(Error).





