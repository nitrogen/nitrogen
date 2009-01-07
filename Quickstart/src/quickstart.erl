-module (quickstart).
-compile(export_all).

%%% APPLICATION FUNCTIONS %%%

start(_, _) -> nitrogen:start().
stop(_) -> nitrogen:stop().
	
% request(_) -> ok.
% route(Path) -> wf_utils:path_to_module(Path).