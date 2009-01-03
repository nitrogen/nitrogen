-module (quickstart).
-compile(export_all).
-export ([init/1]).
-behavior(supervisor).

%%% APPLICATION FUNCTIONS %%%

start(_, _) -> supervisor:start_link(?MODULE, []).
stop(_) -> nitrogen:stop().

%%% SUPERVISOR FUNCTIONS %%%

init(_Args) ->
	Restart = {one_for_one, 1, 1},
	ChildSpec = {quickstart_sup, {nitrogen, start, []}, permanent, 10, worker, dynamic},
	{ok,{Restart,[ChildSpec]}}.
	
% request(_) -> ok.
% route(Path) -> wf_utils:path_to_module(Path).