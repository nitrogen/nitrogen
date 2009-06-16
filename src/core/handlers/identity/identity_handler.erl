% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (identity_handler).
-export ([
	behaviour_info/1, get_user/1, set_user/2, clear/1
]).



% get_user(Context, State) -> User.
% Retrieve an Erlang term representing the current user.
get_user(Context) ->
	_User = wf_context:apply_return_raw(user_handler, get_user, Context).
	
% set_user(User, Context, State) -> {ok, NewContext, NewState}.
% Set an Erlang term representing the current user.
set_user(User, Context) ->
	{ok, _NewContext} = wf_context:apply(user_handler, set_user, [User], Context).
	
% clear(Context, State) -> {ok, NewContext, NewState}.
% Set the user to undefined.
clear(Context) ->
	{ok, _NewContext} = wf_context:apply(user_handler, clear, Context).



behaviour_info(callbacks) -> [
	{init, 2},      
	{finish, 2},
	{get_user, 2},	
	{set_user, 3},
	{clear, 2}	
];
behaviour_info(_) -> undefined.