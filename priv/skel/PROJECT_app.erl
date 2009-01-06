-module (PROJECT_app).
-export ([start/2, stop/1]).
-behavior(application).

start(_, _) -> 
	PROJECT_sup:start_link().

stop(_) -> 
	nitrogen:stop().

