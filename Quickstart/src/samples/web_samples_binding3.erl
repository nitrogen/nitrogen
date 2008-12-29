-module (web_samples_binding3).
-include ("wf.inc").
-compile(export_all).


main() ->	#template { file="./wwwroot/twocolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "Key/Value Pair Binding".
headline() -> "Key/Value Pair Binding".
right() -> linecount:render().

% Binding data stored in key/value pairs.
get_data() -> [
	[{title, "Title 1"}, {author, "Author 1"}, {description, "Description 1"}, {postback, {data, 1}}],
	[{description, "Description 2"}, {title, "Title 2"}, {author, "Author 2"}, {postback, {data, 2}}],
	[{postback, {data, 3}}, {title, "Title 3"}, {author, "Author 3"}, {description, "Description 3"}]
].

get_map() -> [{title, titleLabel@text}, {author, authorLabel@text}, {description, descriptionLabel@text}, {postback, myButton@postback}].

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