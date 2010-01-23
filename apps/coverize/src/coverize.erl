%%% File    : coverize.erl
%%% Author  : Michael Mullis <michael@mullistechnologies.com>
%%% Description : A wrapper for generating cover output with a colorized index and stats
%%% Created : 28 Jan 2009 by Michael Mullis <michael@mullistechnologies.com>
%%% Copyright (c) 2009 Michael Mullis

-module(coverize).
-author("michael@mullistechnologies.com").
-export([run/2,run/4,run/5]).

%% Wrapper for cover to make command line calling easy
-spec(run/2 :: (list(string()), atom()) -> ok).
run(SourceDirs,TestSuiteModule) ->
  CompileOptions = [ debug_info,
                     { i, "./include" },
                     { outdir, "./ebin" }],
    run(SourceDirs,CompileOptions,TestSuiteModule,"./coverage").

%% TestSuiteModule should have a function test/0.
-spec(run/4 :: (list(string()), list(atom()|tuple()), atom(), string()) -> ok).
run(SourceDirs,CompileOptions,TestSuiteModule,OutputDir) ->
  ok = run(SourceDirs,CompileOptions,TestSuiteModule, 'test', OutputDir).

%% Call TestSuiteModule:TestSuiteFunction to run the test
-spec(run/5 :: (list(string()), list(atom()|tuple()), atom(), atom(), string()) -> ok).
run(SourceDirs,CompileOptions,TestSuiteModule,TestSuiteFunction=test, OutputDir) ->
  case cover:start() of
      {error,{already_started,_Pid}} -> ok;
      {error,Error} -> throw(io_lib:format("Could not start cover module: ~p~n",[Error]));
      {ok, _Pid}    -> ok
  end,

  _CompileResults = [cover:compile_directory(X, CompileOptions) || X <- SourceDirs ],

  TestSuiteModule:TestSuiteFunction(),
  SummaryFileName = filename:join([
                                   OutputDir,
                                   "index.html"
                                  ]),

  {ok, SummaryFile} = file:open(SummaryFileName, [write]),

  io:fwrite(SummaryFile, "<html><body><table><th>Module</th><th>Covered</th><th>Not Covered</th><th>% Complete</th>~n", []),

  {_, _, {TotCovered,TotUncovered}} = dump_coverage(OutputDir, SummaryFile, cover:modules()),

  io:fwrite(SummaryFile,
            "<div style=\"margin-right: 0; margin-top:0\">Overall Coverage: ~p% </div>~n",
            [calc_percentage(TotCovered,TotUncovered)]),

  io:fwrite(SummaryFile, "</table></body></html>~n", []),
  ok.

-spec dump_coverage(OutputDir::string(),SummaryFile::pid(),[Module::atom(),...]) -> {'ok',[any()],{number(),number()}}.
dump_coverage(OutputDir, SummaryFile, [Module|RemainingModules]) ->
  dump_coverage(OutputDir, SummaryFile, [Module|RemainingModules], [], {0,0}).

-spec dump_coverage(OutputDir::string(),SummaryFile::pid(),[Module::atom()],RemainingModules::[any()],{number(),number()}) -> {'ok',[any()],{number(),number()}}.
dump_coverage(OutputDir, SummaryFile, [Module|RemainingModules], Acc, {TotCovered,TotUncovered}) ->
  OutputFile = filename:join([
			      OutputDir,
			      atom_to_list(Module)++".COVER.html"
			     ]),
  Acc2 = case cover:analyse_to_file(Module, OutputFile, [html]) of
           {ok, OutFile} ->
             [OutFile|Acc] ;
           {error, _} ->
             Acc
         end,

  {ok,{_name,{CoveredLines,UncoveredLines}}} = cover:analyse(Module,coverage,module),
  TotalsAcc2 = {TotCovered+CoveredLines, TotUncovered+UncoveredLines},

  BackgroundStyle = case UncoveredLines > 0 of
                      true ->
                        "background: #E05070;";  % something red-ish
                      false ->
                        "background: #30d42a;"  % something green-ish
                    end,

  io:fwrite(SummaryFile,
            "<tr style=~p><td><a href=\"~p.COVER.html\">~p</a></td><td style=\"text-align: right\">~p</td><td style=\"text-align: right\">~p</td><td style=\"text-align: center\">~p</td></tr>~n",
            [BackgroundStyle,Module,Module,CoveredLines,UncoveredLines, calc_percentage(CoveredLines,UncoveredLines)]),

  dump_coverage(OutputDir, SummaryFile, RemainingModules, Acc2, TotalsAcc2);

dump_coverage(_, _, [], Acc, TotalsAcc) ->
  {ok, Acc, TotalsAcc}.

-spec calc_percentage(number(),number()) -> integer().
calc_percentage(0,_UncoveredLines) -> 0;
calc_percentage(_,0) ->  100;
calc_percentage(CoveredLines,UncoveredLines) ->
  round((CoveredLines / (CoveredLines+UncoveredLines)) * 100).
