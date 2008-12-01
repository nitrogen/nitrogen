-module (web_binding2).
-include ("wf.inc").
-export ([main/0, event/1]).
-record(mydata, {title, author, description, postback}).

main() ->	
	% Record-based Data...
	Data = [
		#mydata { title="Title 1", author="Author 1", description="Description 1", postback={data, 1} },
		#mydata { title="Title 2", author="Author 2", description="Description 2", postback={data, 2} },
		#mydata { title="Title 3", author="Author 3", description="Description 3", postback={data, 3} }
	],
	Map = #mydata { title=titleLabel@text, author=authorLabel@text, description=descriptionLabel@text, postback=myButton@postback },
	
	Body = #body { title="Record-Based Binding", body=#panel { style="margin: 50px;", body=[
		#h1 { text="Record-Based Binding" },
		#h3 { text="Div Binding" },
		#hr{},
		#bind { id=simpleBinding, data=Data, map=Map, body=[
			#label { id=titleLabel },
			#label { id=authorLabel },
			#label { id=descriptionLabel },
			#button { id=myButton, text="Button" },
			#hr{}
		]},
		
		#h3 { text="Table Binding" },
		#table { rows=[
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
	]}},
	wf:render(Body).
	
event({data, Data}) ->
	Message = "Clicked On Data: " ++ wf:to_list(Data),
	wf:wire(#alert { text=Message }),
	ok;

event(_) -> ok.