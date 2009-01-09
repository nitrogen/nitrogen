{application, quickstart_yaws, [
	{description,  "Nitrogen Quickstart Examples"},
	{mod, {quickstart, []}},
	{env, [
		{platform, yaws},
		{port, 8000},
		{session_timeout, 20},
		{sign_key, "b37ca07"},
		{wwwroot, "./wwwroot"}
	]}
]}.