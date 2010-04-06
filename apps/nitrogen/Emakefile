%%% Compile behaviours first...
{ 
    [	
	"./src/handlers/cache/cache_handler.erl",
	"./src/handlers/config/config_handler.erl",
	"./src/handlers/identity/identity_handler.erl",
	"./src/handlers/log/log_handler.erl",
	"./src/handlers/process_registry/process_registry_handler.erl",
	"./src/handlers/query/query_handler.erl",
	"./src/handlers/role/role_handler.erl",
	"./src/handlers/route/route_handler.erl",
	"./src/handlers/security/security_handler.erl",
	"./src/handlers/session/session_handler.erl",
	"./src/handlers/state/state_handler.erl"
    ], 
    [
        { i, "./include" },
        { outdir, "./ebin" },
        debug_info
    ]
}.


%%% Compile the rest of the code...
{ 
    [	
        "./src/*", 
        "./src/*/*", 
        "./src/*/*/*"
    ], 
    [
        { i, "./include" },
        { outdir, "./ebin" },
        debug_info
    ]
}.
