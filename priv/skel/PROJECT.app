{application, PROJECT, [
	{description,  "Nitrogen Website"},
	{mod, {PROJECT_app, []}},
	{env, [
		{platform, inets}, %% {inets|yaws|mochiweb}
		{signkey, "b37ca07"},
		{wwwroot, "./wwwroot"},
		{port, 8000}
	]}
]}.