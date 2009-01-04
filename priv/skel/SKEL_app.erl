-module (SKEL_app).
-export ([start/2, stop/1]).
-behavior(application).

start(_, _) -> 
	SKEL_sup:start_link().

stop(_) -> 
	nitrogen:stop().

