% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% Contributions from Torbjorn Tornkvist (tobbe@tornkvist.org)
% See MIT-LICENSE for licensing information.

-module (wf_http_basic_auth).
-include ("wf.inc").
-export ([run/2]).

-author('tobbe@tornkvist.org').

%% @doc
%%
%% This module makes use of 3 callback functions which are assumed
%% to be available in the provided CallbackMod module.
%%
%% In order to use it, just call wf_http_basic_auth:run(Module, ?MODULE)
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
%% authenticate(Module, User, Password) -> true.
%%
%% @end


%% Attempt authentication. The general pattern below
%% is to try each step of decoding the authorization string.
%% If the step is successful, then move on to the step below.
%% If the step is NOT successful, then prompt for authentication.

run(Module, CallbackMod) ->
	case wf_platform:get_header(authorization) of
		AuthHeader when AuthHeader /= undefined ->
			check_auth_header(Module, CallbackMod, AuthHeader);
		_ -> 
			prompt_for_authentication(CallbackMod)
	end.

check_auth_header(Module, CallbackMod, AuthHeader) ->
	case string:tokens(AuthHeader, " ") of
		["Basic", Digest] ->
			decode_digest(Module, CallbackMod, Digest);
		_ ->
			prompt_for_authentication(CallbackMod)
	end.
	
decode_digest(Module, CallbackMod, Digest) ->
	case string:tokens(base64:decode_to_string(Digest), ":") of
		[User, Password] ->
			check_is_authenticated(Module, CallbackMod, User, Password);
		_ ->
			prompt_for_authentication(CallbackMod)
	end.
	
check_is_authenticated(Module, CallbackMod, User, Password) ->
	case CallbackMod:is_authenticated(Module, User) of
		false ->
			authenticate_user(Module, CallbackMod, User, Password);
		_ ->
			nitrogen:request(Module)
	end. 

authenticate_user(Module, CallbackMod, User, Password) ->
	case CallbackMod:authenticate(Module, User, Password) of
		true  -> 
			nitrogen:request(Module);
		_ ->
			prompt_for_authentication(CallbackMod)
	end.
	
prompt_for_authentication(CallbackMod) ->
  Realm = CallbackMod:realm(),
  wf_platform:set_header("WWW-Authenticate", "Basic realm=\""++Realm++"\""),
  wf_platform:set_response_code(401),
  "<strong>Authentication required!</strong>".



