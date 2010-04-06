-module(nitrogen_init).
-export ([init/0]).
	
%% Called during application startup.
%% Put other initialization code here.
init() ->
    application:start(mprocreg),
    application:start(nitrogen_mochiweb).
