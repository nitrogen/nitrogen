% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_config_handler).
-include ("wf.inc").
-behaviour (config_handler).
-export ([
    init/2, 
    finish/2,
    get_value/4,
    get_values/4 
]).

init(_Config, _State) -> 
    {ok, []}.

finish(_Config, _State) -> 
    {ok, []}.

get_value(Key, DefaultValue, Config, State) ->
    case get_values(Key, [DefaultValue], Config, State) of
        [Value] -> 
            Value;
        Values ->
            error_logger:error_msg("Too many matching config values for key: ~p~n", [Key]),
            throw({nitrogen_error, too_many_matching_values, Key, Values})
    end.

% First, try to read from the application
% environment. If we don't find the key there
% then try to read from the command line and
% convert to a term.
get_values(Key, DefaultValue, _Config, _State) -> 
    case application:get_env(Key) of
        {ok, Value} -> 
            Value;
        undefined ->
            KeyString = "nitrogen_ " ++ wf:to_list(Key),
            case init:get_argument(KeyString) of
                error -> 
                    DefaultValue;
                {ok, List} -> 
                    F = fun(L, Acc) -> Acc ++ L end,
                    List2 = lists:foldl(F, [], List),
                    [make_term(Key, X) || X <- List2]
            end
    end.

%%% Pulled from application_controller:make_term/1, and shortened.
make_term(Key, Str) -> 
    try
        {ok, Tokens, _} = erl_scan:string(Str ++ "."),
        {ok, Term} = erl_parse:parse_term(Tokens),
        Term
    catch _Type : _Error ->
        error_logger:error_msg("Could not parse term: ~p~n", [Str]),
        throw({nitrogen_error, invalid_config, Key})
    end.
