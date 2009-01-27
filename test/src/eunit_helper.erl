%%% File    : eunit_helper.erl
%%% Author  : Michael Mullis <michael@mullistechnologies.com>
%%% Description : Some helper functions to enhance tests
%%% Created : 27 Jan 2009 by Michael Mullis <michael@mullistechnologies.com>

-module(eunit_helper).
-author("michael@mullistechnologies.com").
-compile([export_all]).  % its just for testing so dont blow a gasket!

%% Compare an Actual value to a Regexp
%% Returns boolen (true for match / false for nomatch)
%% Always uses caseless matching
%% Usage:   ?_assert(regexpMatch("Rock",  "See Rock City").
regexpMatch(Pattern, Actual) ->
  case re:run(Actual, Pattern, [caseless]) of
    {match, [{_Start, _Length}]} ->
      true;
    _ ->
      false
  end.
