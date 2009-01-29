%%% File    : eunit_helper.erl
%%% Author  : Michael Mullis <michael@mullistechnologies.com>
%%% Description : Some helper functions to enhance tests and coverage analysis
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

%% Wrapper for cover to make command line calling easy
run_cover() ->
   %% TODO: Make this list dynamic
  SourceDirs = [
    "./src",
    "./src/actions",
    "./src/elements/forms",
    "./src/elements/heading",
    "./src/elements/html",
    "./src/elements/layout",
    "./src/elements/other",
    "./src/elements/table",
    "./src/lib",
    "./src/platforms",
    "./src/project",
    "./src/validators"
    "./test/src",
    "./test/src/actions",
    "./test/src/elements/html"
  ],

  coverize:run(SourceDirs,test_suite).
