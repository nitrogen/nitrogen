-module (web_test).
-include_lib ("wf.inc").
-export ([main/0, event/1, body/0]).

main() ->
    #template {file="./wwwroot/onecolumn.html"}.

body() ->
		Posts = [
			[<<"1">>, <<"Frank">>],
			[<<"2">>, <<"Bob">>]
		],
    Data = lists:map(fun(L) -> [s(X) || X <- L] end, Posts),
    error_logger:info_msg("Data=~p~n",[hd(Data)]),
    Map = [editLink@postback, fname@text],
    Body = #table { class=tiny, rows=[
              #tablerow { cells=[
                 #tableheader { text="" },
                 #tableheader { text="First Name" }
               ]},
              #bind { id=tableBinding, data=Data, map=Map, 
                      transform = fun alternate_color/2, body=
                  #tablerow { id = top, cells=[
                     #tablecell { body=
                        #link { id=editLink, body=
                            #image{ image="/images/lighten.png"}}},
                     #tablecell { id=fname }
                  ]}
              }]},
    wf:render(Body).

event(E) -> 
    error_logger:info_msg("+++ ~p: ~p~n", [?MODULE, E]),
    ok.


%%% ALTERNATE BACKGROUND COLORS %%%
alternate_color(DataRow, Acc) when Acc == [];
                                    Acc==odd ->  
    {DataRow, even, {top@style, "background-color: #C0C0C0;"}};

alternate_color(DataRow, Acc) when Acc == even ->
  {DataRow, odd, {top@style, "background-color: white;"}}.


s(null)                 -> <<"">>;
s(I) when is_integer(I) -> list_to_binary(gtostr(I));
s(B) when is_binary(B)  -> B.

gtostr(Secs) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
	calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",
				[Year, Month, Day, Hour, Minute, Second])).

    
    
