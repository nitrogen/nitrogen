% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% Created: 29 Jan 2009 by tobbe@tornkvist.org
% See MIT-LICENSE for licensing information.

-module (wf_http_basic_auth).
-include ("wf.inc").
-export ([run/2]).

-author('tobbe@tornkvist.org').

%% @doc
%%
%% This module makes use of 3 callback functions which is assumed
%% to be available in the provided CallbackMod module.
%%
%% In order to use it, just call wf_http_basic_auth:run(Module)
%% as the last function in your AppMod:request/1 function.
%%
%% -spec realm() -> string().    
%%
%% realm/0 - simply return the authentication realm as a string.
%%
%% 
%% -spec is_authenticated(atom(), string()) -> bool().
%%
%% is_authenticated/2 - makes it possible to e.g timeout a session and thus
%% insisting on authentication being performed. If the function returns 'false',
%% the authenticate/2 callback will be called in succession; else no further
%% action will be taken.
%%
%%
%% -spec authenticate(atom(), string(), string()) -> bool().
%%
%% authenticate/3 - will decide if authentication will be requested by the client.
%% In case it returns 'true', no authentication will be requested.
%%
%% EXAMPLES:
%%
%% realm() -> "visitor".
%%
%% is_authenticated(Module, User) -> false.
%%
%% authenticate(web_list, X, X)       -> true;
%% authenticate(web_list, _, _)       -> false;
%% authenticate(Module, User, Passwd) -> true.
%%
%% @end

run(Module, CallbackMod) ->
    try
        ["Basic",Digest] = string:tokens(wf_platform:get_header("authorization"), " "),
        [User,Passwd] = string:tokens(base64:decode_to_string(Digest), ":"),
        case CallbackMod:is_authenticated(Module, User) of
            false -> true = CallbackMod:authenticate(Module, User, Passwd);
            true  -> true
        end,
        nitrogen:request(Module)
    catch
        _:_ -> 
            Realm = CallbackMod:realm(),
            wf_platform:set_header("WWW-Authenticate", "Basic realm=\""++Realm++"\""),
            wf_platform:set_response_code(401),
            "<strong>Authentication required!</strong>"
    end.



