-module (demos_binding1).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos433.html" }.

title() -> "List-Based Binding".

headline() -> "List-Based Binding".

left() -> 
    [
        "
        <p>
        The <code>#bind{}</code> element allows you to bind data to
        one or more elements using an efficient syntax. You define a
        list of data, a list of elements, and a map of where the data
        should be placed within the elements.

        <p>
        This demo binds a list of data to elements.
        ",
        linecount:render()
    ].

% Binding data stored in a simple list.
get_data() -> [
		["Title 1", "Author 1", "Description 1", {data, 1}],
		["Title 2", "Author 2", "Description 2", {data, 2}],
		["Title 3", "Author 3", "Description 3", {data, 3}]		
	].

get_map() -> 
    %% Binding map is positional...
    [
        titleLabel@text, 
        authorLabel@text, 
        descriptionLabel@text, 
        myButton@postback
    ].

middle() -> 
    Data = get_data(),
    Map = get_map(),
    [
        #h2 { text="Div Binding" },
        #bind { id=simpleBinding, data=Data, map=Map, body=[
            #hr{},
            #label { class=tiny, id=titleLabel },
            #label { class=tiny, id=authorLabel },
            #label { class=tiny, id=descriptionLabel },
            #button { class=tiny, id=myButton, text="Button" }
        ]}
    ].

right() ->	
    Data = get_data(),
    Map = get_map(),
    [
        #h2 { text="Table Binding" },
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
    ].

event({data, Data}) ->
    Message = "Clicked On Data: " ++ wf:to_list(Data),
    wf:wire(#alert { text=Message }),
    ok;

event(_) -> ok.
