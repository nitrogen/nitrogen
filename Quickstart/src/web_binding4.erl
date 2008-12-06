-module (web_binding4).
-include ("wf.inc").
-export ([main/0, event/1]).


main() ->	
  % List-based Data...
	Data = [
		["Title 1", "Author 1", "Description 1", {data, 1}],
		["Title 2", "Author 2", "Description 2", {data, 2}],
		["Title 3", "Author 3", "Description 3", {data, 3}],
		["Title 4", "Author 4", "Description 4", {data, 4}],	
		["Title 5", "Author 5", "Description 5", {data, 5}]	
	],
	Map = [titleLabel@text, authorLabel@text, descriptionLabel@text, myButton@postback],
		
	Title = "Binding With Transform",
	Body = #template { file=twocolumn, title=Title, headline=Title,
		section1=[
			#h3 { text="Div Binding" },
			"
			Use a transform function to make the data alternately uppercase and lowercase.<br>
			",
			#bind { id=simpleBinding, data=Data, map=Map, transform=fun alternate_case/2, body=[
				#hr{},
				#label { class=tiny, id=titleLabel },
				#label { class=tiny, id=authorLabel },
				#label { class=tiny, id=descriptionLabel },
				#button { class=tiny, id=myButton, text="Button" }
			]}
		],
		
		section2=[
			#h3 { text="Table Binding" },
			"
			Use a transform function to make the rows different colors.<br>
			",
			#table { class=tiny, rows=[
				#tablerow { cells=[
					#tableheader { text="Title" },
					#tableheader { text="Author" },
					#tableheader { text="Description" },
					#tableheader { }
				]},
				#bind { id=tableBinding, data=Data, map=Map, transform=fun alternate_color/2, body=#tablerow { id=top, cells=[
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

%%% ALTERNATE CASE %%%
alternate_case(DataRow, Acc) when Acc == []; Acc == odd  ->
	[Title, Author, Description, Postback] = DataRow,
	F = fun string:to_upper/1,
	{ [F(Title), F(Author), F(Description), Postback], even, [] };

alternate_case(DataRow, Acc) when Acc == even  ->
	[Title, Author, Description, Postback] = DataRow,
	F = fun string:to_lower/1,
	{ [F(Title), F(Author), F(Description), Postback], odd, [] }.


%%% ALTERNATE BACKGROUND COLORS %%%
alternate_color(DataRow, Acc) when Acc == []; Acc==odd ->
	{DataRow, even, {top@style, "background-color: #eee;"}};

alternate_color(DataRow, Acc) when Acc == even ->
	{DataRow, odd, {top@style, "background-color: #ddd;"}}.