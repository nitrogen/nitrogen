{application, quickstart_inets, [
	{description,  "Nitrogen Quickstart Examples"},
	{mod, {quickstart, []}},
	{env, [
		{platform, inets}, % overridden by quickstart* scripts.
		{signkey, "b37ca07"},
		{wwwroot, "./wwwroot"},
		{port, 8000}
	]}
]}.