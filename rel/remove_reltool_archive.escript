#!/usr/bin/env escript
%% -*- erlang -*-
%% vim: ts=4 sw=4 et ft=erlang
%%
%% Coyright 2024 - Jesse Gumm
%% MIT License
%%
%%
%% This looks at a reltool.config file and if the OTP version is 26 or higher,
%% it removes the `excl_archive_filters` key, since that was removed in OTP 26.
%%
main([File]) ->
    main([File,File]);
main([FromFile,ToFile]) ->
    {ok,Terms} = file:consult(FromFile),
    OTP26 = otp_26_or_higher(),
    FinalTerms = case OTP26 of
        false ->
            Terms;
        true ->
            Sys = proplists:get_value(sys, Terms),
            Sys2 = lists:keydelete(excl_archive_filters, 1, Sys),
            SysFinal = Sys2,

            Terms2 = proplists:delete(sys, Terms),
            [{sys, SysFinal} | Terms2]
    end,

    FormattedTerms = [io_lib:format("~p.~n",[T]) || T <- FinalTerms],

    if
        FromFile=:=ToFile, OTP26 ->
            io:format("Making ~p compatible with OTP 26+~n", [FromFile]);
        FromFile=:=ToFile ->
            io:format("No changes necessary to ~p for OTP < 26+~n", [FromFile]);
        OTP26 ->
            io:format("Making ~p compatible with OTP 26+ (from ~p)~n", [ToFile, FromFile]);
        true ->
            io:format("Copying ~p to ~p, no OTP <26 compatibility changes needed~n",[FromFile, ToFile])
    end,
    file:write_file(ToFile, FormattedTerms).

otp_26_or_higher() ->
    try list_to_integer(erlang:system_info(otp_release)) of
        Vsn -> Vsn >= 26
    catch
        _:_ -> false
    end.
