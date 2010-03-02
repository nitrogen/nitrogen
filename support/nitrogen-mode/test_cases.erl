-module(test_cases).
-include ("wf.inc").
-compile(export_all).

main() ->
    %% Test records...
    A = #record {},
    A = #record {
      a=1,
      b=2
     },
    A = #record { a=1 
                },

    A = #record { a=#record {
                    a=1, b=2
                   }},

    Elements = #panel { body=[
        #span { text="Hello, World!" }
    ]},

    #record { a=[
                 a, b, c
                ]},

    %% Test case statements...
    case a of a -> ok 
    end,

    case a of a -> ok; b-> ok
    end,

    case a of 
	a -> ok;
	b -> ok
    end,

    case a of 
	a ->
	    ok;
	b ->
	    ok
    end,

    case a of
	a -> {
          a, b, c
         }
    end,

    %% Test Try/Catch
    try a catch _ : _ -> ok end,
    try a catch _ : _ -> 
                  ok 
          end,

    try 
	try 
	    try 
		a
	    catch _ : _ -> ok 
	    end
	catch _ : _ ->
                ok
	end
    end,  

    try a 
    catch _ : _ -> ok 
    end,

    try 
	a 
    catch _ : _ -> ok 
    end,

    try a catch 
              _ : _ -> ok 
          end,

    try a of a -> ok
    catch _ : _ -> ok
    after 100 -> test
    end,

    try a of 
	a -> ok
    catch _ : _ -> ok
    after 100 -> test
    end,

    try a of 
	a -> 
	    ok
    catch _ : _ -> ok
    after 100 -> test
    end,

    try a of a -> ok
    catch _ : _ -> ok
    after 
	100 -> test
    end,

    try a of 
	a -> 
	    ok
    catch 
	_ : _ -> 
	    ok
    after 100 -> test
    end,

    try a of a -> ok
    catch _ : _ -> ok
    after 100 -> 
            test
    end,

    try a of a -> ok
    catch _ : _ -> ok
    after 
	100 -> 
	    test
    end,

    %% Function
    F = fun() -> ok end,

    F = fun() -> ok
        end,

    F = fun() ->
                ok
        end,

    %% List
    A = [
         a,
         b
        ],

                                                % String
    A = "
    This is a string
        ",

    Pid = spawn(fun() ->
                        hello 
                end),    

    Pid = spawn(fun() ->
                        ok 
                end),

    Pid = spawn_link(fun() ->
                             ok
                     end),



    [
     test
     ||
	lists:seq(1, 10)
    ],

    case test of
	true ->
	    [ test
              || X <- lists:seq(1, 5)];
	false ->
	    [begin
                 X
             end || 
		X <- lists:seq(1, 5)
	    ]
    end,		

    ok.

