{application, quickstart_inets, [
	{description,  "Nitrogen Quickstart Examples"},
	{mod, {quickstart_inets_app, []}},
	{env, [
		{port, 8000},
		{session_timeout, 20},
		{sign_key, "b37ca07"},
		{wwwroot, "./wwwroot"}
	]}
]}.