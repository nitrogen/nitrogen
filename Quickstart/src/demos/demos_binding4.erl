-module (demos_binding4).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos433.html" }.

title() -> "Binding with a Transform Function".

headline() -> "Binding with a Transform Function".

left() -> 
    [
        "
        <p>
        The <code>#bind{}</code> element allows you to bind data to
        one or more elements using an efficient syntax. You define a
        list of data, a list of elements, and a map of where the data
        should be placed within the elements.

        <p>
        Optionally, you can specify a transform function on a
        <code>#bind{}</code> element.  A transform function takes two
        parameters, the row of data, and an accumulator, and can be
        used to manipulate the data before it is ultimately bound to
        the list of elements.

        <p>
        This demo shows two transform functions in action, one to
        toggle the case of the supplied data, and one to change the
        row color.
        ",
        linecount:render()
    ].

% Binding data with a transform function.
get_data() -> [
	["Title 1", "Author 1", "Description 1", {data, 1}],
	["Title 2", "Author 2", "Description 2", {data, 2}],
	["Title 3", "Author 3", "Description 3", {data, 3}],
	["Title 4", "Author 4", "Description 4", {data, 4}],	
	["Title 5", "Author 5", "Description 5", {data, 5}]	
].

get_map() -> 
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
    ].

right() -> 
    Data = get_data(),
    Map = get_map(),
    [
        #h2 { text="Table Binding" },
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
    ].

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
