-module (web_binding1).
-include ("wf.inc").
-export ([main/0, event/1]).


main() ->	
  % List-based Data...
	Data = [
		["Title 1", "Author 1", "Description 1", {data, 1}],
		["Title 2", "Author 2", "Description 2", {data, 2}],
		["Title 3", "Author 3", "Description 3", {data, 3}]		
	],
	Map = [titleLabel@text, authorLabel@text, descriptionLabel@text, myButton@postback],
	
	Title = "Simple Binding",
	Body = #template { file=twocolumn, title=Title, headline=Title,
		section1=[
			#h3 { text="Div Binding" },
			#bind { id=simpleBinding, data=Data, map=Map, body=[
				#hr{},
				#label { class=tiny, id=titleLabel },
				#label { class=tiny, id=authorLabel },
				#label { class=tiny, id=descriptionLabel },
				#button { class=tiny, id=myButton, text="Button" }
			]}
		],
		section2=[
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
		]
	},
	wf:render(Body).
	
event({data, Data}) ->
	Message = "Clicked On Data: " ++ wf:to_list(Data),
	wf:wire(#alert { text=Message }),
	ok;

event(_) -> ok.