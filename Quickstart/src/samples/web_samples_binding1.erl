-module (web_samples_binding1).
-include ("wf.inc").
-compile(export_all).

main() ->	#template { file="./wwwroot/twocolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "Simple Binding".
headline() -> "Simple Binding".
right() -> linecount:render().

% Binding data stored in a simple list.
get_data() -> [
		["Title 1", "Author 1", "Description 1", {data, 1}],
		["Title 2", "Author 2", "Description 2", {data, 2}],
		["Title 3", "Author 3", "Description 3", {data, 3}]		
	].

get_map() -> [titleLabel@text, authorLabel@text, descriptionLabel@text, myButton@postback].

column1() -> 
	Data = get_data(),
	Map = get_map(),
	Column1 = [
		#h3 { text="Div Binding" },
		#bind { id=simpleBinding, data=Data, map=Map, body=[
			#hr{},
			#label { class=tiny, id=titleLabel },
			#label { class=tiny, id=authorLabel },
			#label { class=tiny, id=descriptionLabel },
			#button { class=tiny, id=myButton, text="Button" }
		]}
	],
	wf:render(Column1).

column2() ->	
	Data = get_data(),
	Map = get_map(),
	Column2 = [
		#h3 { text="Table Binding" },
		#table { class=tiny, rows=[
			#tablerow { cells=[
				#tableheader { text="Title" },
				#tableheader { text="Author" },
				#tableheader { text="Description" },
				#tableheader { }
			]},
			#bind { id=tableBinding, data=Data, map=Map, body=#tablerow { cells=[
				#tablecell { id=titleLabel },
				#tablecell { id=authorLabel },
				#tablecell { id=descriptionLabel },
				#tablecell { body=#button { id=myButton, text="Button" } }
			]}}
		]}
	],
	wf:render(Column2).
	
event({data, Data}) ->
	Message = "Clicked On Data: " ++ wf:to_list(Data),
	wf:wire(#alert { text=Message }),
	ok;

event(_) -> ok.