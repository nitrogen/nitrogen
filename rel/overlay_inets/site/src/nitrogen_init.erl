-module(nitrogen_init).
-export ([init/0]).
	
%% Called during application startup.
%% Put other initialization code here.
init() ->
    application:start(nprocreg).
