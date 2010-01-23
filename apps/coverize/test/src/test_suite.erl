-module(test_suite).

-author("michael@mullistechnologies.com").

-spec test() -> any().
-include_lib("eunit/include/eunit.hrl").

-spec all_test_() -> [{'module','simple_test'},...].
all_test_() ->
  [{module, simple_test}
  ].
