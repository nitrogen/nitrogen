-module (PROJECT_app).
-export ([start/2, stop/1]).
-behavior(application).

start(_, _) -> 
	nitrogen:start().

stop(_) -> 
	nitrogen:stop().

