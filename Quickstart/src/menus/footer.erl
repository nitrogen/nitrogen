-module (footer).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

render() -> 
	LearnItems = leftmenu:get_items(learn), 
	DiscussItems =leftmenu:get_items(discuss), 
	ReferenceItems =leftmenu:get_items(reference), 
	CodeItems = leftmenu:get_items(code), 
	Map = {ignore, link@text, link@url},
	Body = #listitem { body=#link { id=link } },
	
	#list { body=[
		#listitem { body=[
			"Learn", 
			#list { body=#bind { data=LearnItems, map=Map, body=Body }}
		]},

		#listitem { body=[
			"Discuss",
			#list { body=#bind { data=DiscussItems, map=Map, body=Body }}
		]},

		#listitem { body=[
			"Reference",
			#list { body=#bind { data=ReferenceItems, map=Map, body=Body }}
		]},

		#listitem { body=[
			"Code", 
			#list { body=#bind { data=CodeItems, map=Map, body=Body }}
		]}
	]}.
