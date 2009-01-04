-module (SKEL_sup).
-behaviour (supervisor).
-export ([start_link/0, init/1]).

start_link() ->
	supervisor:start_link(?MODULE, []).

init(_Args) ->
	Restart = {one_for_one, 1, 1},
	ChildSpec = {SKEL, {nitrogen, start, []}, permanent, 10, worker, dynamic},
	{ok,{Restart,[ChildSpec]}}.
