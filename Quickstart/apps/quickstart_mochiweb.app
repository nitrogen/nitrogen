{application, quickstart_mochiweb, [
	{description,  "Nitrogen Quickstart Examples"},
	{mod, {quickstart_mochiweb_app, []}},
	{env, [
		{ip, {0,0,0,0}},
		{port, 8000},
		{wwwroot, "./wwwroot"}
	]}
]}.