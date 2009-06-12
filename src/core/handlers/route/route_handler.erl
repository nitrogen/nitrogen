% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (route_handler).
-export ([
	behaviour_info/1,
	route/1
]).



% route(Context, State) -> {ok, NewContext, NewState}.
% Route the request by setting the module and path_info 
% attributes of Context#context.page_context.
route(Context) ->
	wf_context:apply(route, route, Context).



behaviour_info(callbacks) -> [
	{init, 2},      
	{finish, 2},
	{route, 2}
];

behaviour_info(_) -> undefined.