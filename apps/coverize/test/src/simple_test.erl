-module(simple_test).

-author("michael@mullistechnologies.com").

-spec test() -> any().
-include_lib("eunit/include/eunit.hrl").

-spec basic_test_() -> [{integer(),fun(() -> 'ok')},...].
basic_test_() ->
  [?_assertEqual(" ", " " )
  ].
