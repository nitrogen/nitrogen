% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (route_handler).
-export ([
	behaviour_info/1
]).

behaviour_info(callbacks) -> [
	{init, 1},      
	{finish, 1}
];

behaviour_info(_) -> undefined.