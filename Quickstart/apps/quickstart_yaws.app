{application, quickstart_yaws, [
	{description,  "Nitrogen Quickstart Examples"},
	{mod, {quickstart, []}},
	{env, [
		{platform, yaws},
		{signkey, "b37ca07"},
		{wwwroot, "./wwwroot"},
		{port, 8000}
	]}
]}.