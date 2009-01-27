-module(test_suite).

-author("michael@mullistechnologies.com").

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  [{module, element_br_test},
   {module, element_link_test}
  ].
