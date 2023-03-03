#!/usr/bin/env escript
%% -*- erlang -*-
%% vim: ts=4 sw=4 et ft=erlang
%%
%% Coyright 2023 - Jesse Gumm
%%
%% MIT License
%%
%% The Version calculations were lifted from relx:
%% https://github.com/erlware/relx/blob/6a9eae01662e72f403f2f12967332727dd422678/src/rlx_config.erl#L229

-define(VSN_PREVIOUS, "version.previous").
-define(VSN_OLD_PREVIOUS, ?VSN_PREVIOUS ++ ".old").
-define(VSN_CURRENT, "version.current").
-define(VSN_HISTORY, "version.history").
-define(VSN_LOCK, "version.lock").

main(["go"]) ->
    case filelib:is_regular(?VSN_LOCK) of
        true ->
            io:format("Cannot proceed. Previous version attempt failed.  You probably want to run this with the revert option~n"),
            halt(1);
        false ->
            VsnString = get_version_string(),
            OldVsn = read_file(?VSN_CURRENT),
	    io:format("OldVsn: ~s~n",[OldVsn]),
            case VsnString==OldVsn of
                true ->
                    io:format("No version change~n");
                false ->
                    VsnChange = [OldVsn," => ",VsnString],
                    io:format("Version Change: ~s~n",[VsnChange]),
                    io:format("Saving new version file: ~s.~n",[?VSN_CURRENT]),
                    file:rename(?VSN_PREVIOUS, ?VSN_OLD_PREVIOUS),
                    file:rename(?VSN_CURRENT, ?VSN_PREVIOUS),
                    file:write_file(?VSN_CURRENT, VsnString),
                    io:format("Locking Version while release is being generated...~n"),
                    file:write_file(?VSN_LOCK, "")
            end
        end;
main(["revert"]) ->
    case filelib:is_regular(?VSN_LOCK) of
        true ->
            file:rename(?VSN_PREVIOUS, ?VSN_CURRENT),
            file:rename(?VSN_OLD_PREVIOUS, ?VSN_PREVIOUS),
            file:delete(?VSN_LOCK),
            io:format("Version File successfully reverted~n");
        false ->
            io:format("The version is not currently locked~n"),
            halt(1)
    end;
main(["finish"]) ->
    case filelib:is_regular(?VSN_LOCK) of
        true ->
            NewVsn = read_file(?VSN_CURRENT),
            OldVsn = read_file(?VSN_PREVIOUS),
            VsnChange = [OldVsn," => ",NewVsn],
	    TS = timestamp(),
            file:write_file(?VSN_HISTORY, ["(",TS,"): ",VsnChange, "\n"], [append]),
            file:delete(?VSN_OLD_PREVIOUS),
            file:delete(?VSN_LOCK),
            io:format("Version Unlocked~n");
        false ->
            io:format("The version is not currently locked, but that is okay~n")
    end;
main(_) ->
    ScriptName = escript:script_name(),
    io:format("Usage:
    ./~s go
    ./~s finish
    ./~s revert~n", [ScriptName, ScriptName, ScriptName]),
    halt(1).

read_file(F) ->
    case file:read_file(F) of
        {ok, X} -> chomp(binary_to_list(X));
        {error, _} -> "first-run"
    end.

chomp(X) ->
	string:chomp(X).

get_version_string() ->
    V = case in_git() of
        true ->
            {Vsn, RawRef, RawCount} = collect_default_refcount(),
            build_vsn_string(Vsn, RawRef, RawCount);
        false ->
           os:cmd("date +0.%Y%m%d.%H%M.%S")
    end,
    chomp(V).


in_git() ->
    case os:cmd("./in-git.sh") of
        "yes" ++ _ -> true;
        _ -> false
    end.

collect_default_refcount() ->
    %% Get the tag timestamp and minimal ref from the system. The
    %% timestamp is really important from an ordering perspective.
    RawRef = os:cmd("git log -n 1 --pretty=format:'%h\n' "),
    
    case parse_tags() of
        {"0", "0"} ->
            TotalCommitCount = os:cmd("git rev-list --all --count"),
            {"0.0.0", RawRef, TotalCommitCount};
        {Tag, Vsn} ->
            io:format("Tag: ~p. Vsn: ~p~n",[Tag, Vsn]),
            RawCount = get_patch_count(Tag),
            {Vsn, RawRef, RawCount}
    end.

get_patch_count(RawRef) ->
    Ref = re:replace(RawRef, "\\s", "", [global]),
    Cmd = io_lib:format("git rev-list --count ~s..HEAD", [Ref]),
    os:cmd(Cmd).

build_vsn_string(Vsn, RawRef, RawCount) ->
    %% Cleanup the tag and the Ref information. Basically leading 'v's and
    %% whitespace needs to go away.
    RefTag = [".ref", re:replace(RawRef, "\\s", "", [global])],
    Count = re:replace(RawCount, "\\D", "", [global]),

    %% Create the valid [semver](http://semver.org) version from the tag
    case iolist_to_binary(Count) of
        <<"0">> ->
            lists:flatten(Vsn);
        CountBin ->
            binary_to_list(iolist_to_binary([Vsn, "+build.", CountBin, RefTag]))
    end.

parse_tags() ->
    case os:cmd("git describe --abbrev=0 --tags") of
        "fatal:" ++ _ ->
            {"0", "0"};
        Tag ->
            Vsn = string:trim(string:trim(Tag, leading, "v"), trailing, "\n"),
            {Tag, Vsn}
    end.

timestamp() ->
    calendar:system_time_to_rfc3339(os:system_time(second)).
