{application, SKEL, [
	{description,  "Nitrogen Website"},
	{mod, {SKEL_app, []}},
	{env, [
		{platform, inets}, %% {inets|yaws|mochiweb}
		{signkey, "b37ca07"},
		{wwwroot, "./wwwroot"},
		{port, 8000}
	]}
]}.