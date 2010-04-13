-module (demos_binding2).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

-record(mydata, {title, author, description, postback}).

main() -> #template { file="./templates/demos433.html" }.

title() -> "Record-Based Binding".

headline() -> "Record-Based Binding".

left() -> 
    [
 	"
        <p>
        The <code>#bind{}</code> element allows you to bind data to
        one or more elements using an efficient syntax. You define a
        list of data, a list of elements, and a map of where the data
        should be placed within the elements.

        <p>
        This demo binds an Erlang record to elements.
        ",
        linecount:render()
    ].

% Binding data stored in a record.
get_data() -> [
    #mydata { title="Title 1", author="Author 1", description="Description 1", postback={data, 1} },
    #mydata { title="Title 2", author="Author 2", description="Description 2", postback={data, 2} },
    #mydata { title="Title 3", author="Author 3", description="Description 3", postback={data, 3} }
].

get_map() -> 
    %% The binding map is field based...
    #mydata { 
        title=titleLabel@text, 
        author=authorLabel@text, 
        description=descriptionLabel@text, 
        postback=myButton@postback 
    }.

middle() -> 
    Data = get_data(),
    Map = get_map(),
    Column1 = [
        #h2 { text="Div Binding" },
        #bind { id=simpleBinding, data=Data, map=Map, body=[
            #hr{},
            #label { class=tiny, id=titleLabel },
            #label { class=tiny, id=authorLabel },
            #label { class=tiny, id=descriptionLabel },
            #button { class=tiny, id=myButton, text="Button" }
        ]}	
    ],
    Column1.

right() -> 	
    Data = get_data(),
    Map = get_map(),
    Column2 = [
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
    ],
    Column2.

event({data, Data}) ->
    Message = "Clicked On Data: " ++ wf:to_list(Data),
    wf:wire(#alert { text=Message }),
    ok;

event(_) -> ok.
