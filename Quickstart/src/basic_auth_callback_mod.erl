%%%-------------------------------------------------------------------
%%% @author Manuel Durán Aguete <manueld@caixagalicia.es>
%%% @copyright (C) 2010, Manuel Durán Aguete
%%% @doc
%%%
%%% @end
%%% Created :  2 Jun 2010 by Manuel Durán Aguete <manueld@caixagalicia.es>
%%%-------------------------------------------------------------------
-module(basic_auth_callback_mod).
-include_lib ("nitrogen/include/wf.hrl").

%% API
-export([realm/0,
	is_protected/1,
	is_authenticated/2,
	authenticate/3
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
realm() ->
   "Protected Area".

is_protected(Page) -> 
?LOG("Module ~p ",[Page]),
false.

is_authenticated(Module, User) -> 
true.

authenticate(Module, User, Password) -> 
?LOG("Module ~p User ~p Password ~p ",[Module,User, Password]),
true.

%%%===================================================================
%%% Internal functions
%%%===================================================================
