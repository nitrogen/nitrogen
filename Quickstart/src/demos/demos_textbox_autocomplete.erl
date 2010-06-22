-module (demos_textbox_autocomplete).
-include_lib ("nitrogen/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "Autocompletion".

headline() -> "Autocompletion".

left() -> 
    [
        "
        <p>
        Please add documentation!
        ",
        linecount:render()
    ].

right() -> 
    [
        #p{},
        #label { text="What's your favorite programming language?" },
        #textbox_autocomplete { tag=auto1 },
        #flash {}
    ].

autocomplete_enter_event(SearchTerm, _Tag) ->
    Data = [
      {struct, [{id, <<"perl">>}, {label, <<"Perl">>} , {value, <<"Perl">> }]},
      {struct, [{id, <<"php">>}, {label, <<"PHP">>} , {value, <<"PHP">> }]},
      {struct, [{id, <<"erlang">>}, {label, <<"Erlang">>} , {value, <<"Erlang">> }]},
      {struct, [{id, <<"ruby">>}, {label, <<"Ruby">>} , {value, <<"Ruby">> }]},
      {struct, [{id, <<"scala">>}, {label, <<"Scala">>} , {value, <<"Scala">> }]}
    ],
    List = [{struct,[{id, Id }, {label, Label}, {value, Value}]} || 
      {struct,[{id, Id }, {label, Label}, {value, Value}]} <- Data, 
      string:str(string:to_lower(binary_to_list(Label)), string:to_lower(SearchTerm)) > 0
    ],
    mochijson2:encode(List).

autocomplete_select_event({struct, [{<<"id">>, _ },{<<"value">>, Value}]} , _Tag) ->
    wf:flash(Value),
    ok.

event(_) -> ok.
