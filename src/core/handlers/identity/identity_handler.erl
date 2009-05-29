% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (identity_handler).
-export ([
	behaviour_info/1
]).

behaviour_info(callbacks) -> [
	% init(Context) -> {ok, NewContext, NewState}.
	% Called at the start of the request.
	{init, 1},      

	% finish(Context, State) -> {ok, NewContext, NewState}.
	% Called at the end of the request, before sending
	% a response back to the browser.
	{finish, 2},
	
	% user(Context, State) -> {ok, User, NewContext, NewState}.
	% Retrieve an Erlang term representing the current user.
	{get_user, 2},
	
	% user(User, Context, State) -> {ok, NewContext, NewState}.
	% Set an Erlang term representing the current user.
	{set_user, 3},
	
	% logout(Context, State) -> {ok, NewContext, NewState}.
	% Set the user to undefined.
	{logout, 2}	
];

behaviour_info(_) -> undefined.
