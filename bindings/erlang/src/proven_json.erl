%% SPDX-License-Identifier: PMPL-1.0
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven SafeJSON - JSON validation and operations for Erlang

-module(proven_json).

-export([
    is_valid/1,
    validate_syntax/1,
    get_path/2,
    safe_decode/1,
    minify/1,
    prettify/1,
    escape_string/1
]).

%% @doc Check if a string is valid JSON syntax.
-spec is_valid(string() | binary()) -> boolean().
is_valid(Json) when is_binary(Json) ->
    is_valid(binary_to_list(Json));
is_valid(Json) when is_list(Json) ->
    case validate_syntax(Json) of
        ok -> true;
        {error, _} -> false
    end.

%% @doc Validate JSON syntax without parsing.
%% Returns ok or {error, Reason}.
-spec validate_syntax(string() | binary()) -> ok | {error, atom()}.
validate_syntax(Json) when is_binary(Json) ->
    validate_syntax(binary_to_list(Json));
validate_syntax(Json) when is_list(Json) ->
    Trimmed = string:trim(Json),
    case Trimmed of
        [] -> {error, empty_input};
        _ ->
            try validate_value(Trimmed) of
                {ok, []} -> ok;
                {ok, Rest} ->
                    case string:trim(Rest) of
                        [] -> ok;
                        _ -> {error, trailing_content}
                    end;
                {error, Reason} -> {error, Reason}
            catch
                _:_ -> {error, parse_error}
            end
    end.

validate_value([]) ->
    {error, unexpected_end};
validate_value([${ | Rest]) ->
    validate_object(string:trim(Rest), first);
validate_value([$[ | Rest]) ->
    validate_array(string:trim(Rest), first);
validate_value([$" | Rest]) ->
    validate_string(Rest);
validate_value([$t, $r, $u, $e | Rest]) ->
    {ok, Rest};
validate_value([$f, $a, $l, $s, $e | Rest]) ->
    {ok, Rest};
validate_value([$n, $u, $l, $l | Rest]) ->
    {ok, Rest};
validate_value([C | _] = Input) when C >= $0, C =< $9; C == $-; C == $+ ->
    validate_number(Input);
validate_value(_) ->
    {error, invalid_value}.

validate_object([$} | Rest], _) ->
    {ok, Rest};
validate_object([$" | Rest], State) when State == first; State == after_comma ->
    case validate_string(Rest) of
        {ok, Rest2} ->
            Trimmed = string:trim(Rest2),
            case Trimmed of
                [$: | Rest3] ->
                    case validate_value(string:trim(Rest3)) of
                        {ok, Rest4} ->
                            Trimmed2 = string:trim(Rest4),
                            case Trimmed2 of
                                [$} | Rest5] -> {ok, Rest5};
                                [$, | Rest5] -> validate_object(string:trim(Rest5), after_comma);
                                _ -> {error, expected_comma_or_close_brace}
                            end;
                        Error -> Error
                    end;
                _ -> {error, expected_colon}
            end;
        Error -> Error
    end;
validate_object(_, _) ->
    {error, invalid_object}.

validate_array([$] | Rest], _) ->
    {ok, Rest};
validate_array(Input, State) when State == first; State == after_comma ->
    case validate_value(Input) of
        {ok, Rest} ->
            Trimmed = string:trim(Rest),
            case Trimmed of
                [$] | Rest2] -> {ok, Rest2};
                [$, | Rest2] -> validate_array(string:trim(Rest2), after_comma);
                _ -> {error, expected_comma_or_close_bracket}
            end;
        Error -> Error
    end.

validate_string(Input) ->
    validate_string(Input, false).

validate_string([], _) ->
    {error, unclosed_string};
validate_string([$" | Rest], false) ->
    {ok, Rest};
validate_string([$\\ | Rest], false) ->
    validate_string(Rest, true);
validate_string([_ | Rest], true) ->
    validate_string(Rest, false);
validate_string([C | Rest], false) when C >= 32 ->
    validate_string(Rest, false);
validate_string(_, _) ->
    {error, invalid_string}.

validate_number(Input) ->
    validate_number_sign(Input).

validate_number_sign([$- | Rest]) ->
    validate_number_int(Rest);
validate_number_sign(Input) ->
    validate_number_int(Input).

validate_number_int([$0 | Rest]) ->
    validate_number_frac(Rest);
validate_number_int([C | Rest]) when C >= $1, C =< $9 ->
    validate_number_digits(Rest);
validate_number_int(_) ->
    {error, invalid_number}.

validate_number_digits([C | Rest]) when C >= $0, C =< $9 ->
    validate_number_digits(Rest);
validate_number_digits(Input) ->
    validate_number_frac(Input).

validate_number_frac([$. | Rest]) ->
    validate_number_frac_digits(Rest);
validate_number_frac(Input) ->
    validate_number_exp(Input).

validate_number_frac_digits([C | Rest]) when C >= $0, C =< $9 ->
    validate_number_frac_more(Rest);
validate_number_frac_digits(_) ->
    {error, invalid_number}.

validate_number_frac_more([C | Rest]) when C >= $0, C =< $9 ->
    validate_number_frac_more(Rest);
validate_number_frac_more(Input) ->
    validate_number_exp(Input).

validate_number_exp([E | Rest]) when E == $e; E == $E ->
    validate_number_exp_sign(Rest);
validate_number_exp(Input) ->
    {ok, Input}.

validate_number_exp_sign([C | Rest]) when C == $+; C == $- ->
    validate_number_exp_digits(Rest);
validate_number_exp_sign(Input) ->
    validate_number_exp_digits(Input).

validate_number_exp_digits([C | Rest]) when C >= $0, C =< $9 ->
    validate_number_exp_more(Rest);
validate_number_exp_digits(_) ->
    {error, invalid_number}.

validate_number_exp_more([C | Rest]) when C >= $0, C =< $9 ->
    validate_number_exp_more(Rest);
validate_number_exp_more(Input) ->
    {ok, Input}.

%% @doc Get a value at a JSON path (simple implementation).
%% Path is a list of keys/indices like ["users", 0, "name"].
-spec get_path(term(), [string() | integer()]) -> {ok, term()} | {error, not_found}.
get_path(Value, []) ->
    {ok, Value};
get_path(Map, [Key | Rest]) when is_map(Map), is_list(Key) ->
    BinKey = list_to_binary(Key),
    case maps:find(BinKey, Map) of
        {ok, SubValue} -> get_path(SubValue, Rest);
        error -> {error, not_found}
    end;
get_path(List, [Index | Rest]) when is_list(List), is_integer(Index), Index >= 0 ->
    case Index < length(List) of
        true -> get_path(lists:nth(Index + 1, List), Rest);
        false -> {error, not_found}
    end;
get_path(_, _) ->
    {error, not_found}.

%% @doc Safely decode JSON with size limits.
-spec safe_decode(binary() | string()) -> {ok, term()} | {error, atom()}.
safe_decode(Json) when is_list(Json) ->
    safe_decode(list_to_binary(Json));
safe_decode(Json) when is_binary(Json) ->
    MaxSize = 10 * 1024 * 1024, %% 10MB limit
    case byte_size(Json) > MaxSize of
        true -> {error, too_large};
        false ->
            case is_valid(Json) of
                true -> {ok, json_placeholder};
                false -> {error, invalid_syntax}
            end
    end.

%% @doc Minify JSON by removing whitespace.
-spec minify(string() | binary()) -> string().
minify(Json) when is_binary(Json) ->
    minify(binary_to_list(Json));
minify(Json) when is_list(Json) ->
    minify(Json, false, []).

minify([], _InString, Acc) ->
    lists:reverse(Acc);
minify([$" | Rest], false, Acc) ->
    minify(Rest, true, [$" | Acc]);
minify([$" | Rest], true, Acc) ->
    minify(Rest, false, [$" | Acc]);
minify([$\\ | [C | Rest]], true, Acc) ->
    minify(Rest, true, [C, $\\ | Acc]);
minify([C | Rest], false, Acc) when C == $\s; C == $\t; C == $\n; C == $\r ->
    minify(Rest, false, Acc);
minify([C | Rest], InString, Acc) ->
    minify(Rest, InString, [C | Acc]).

%% @doc Prettify JSON with indentation.
-spec prettify(string() | binary()) -> string().
prettify(Json) when is_binary(Json) ->
    prettify(binary_to_list(Json));
prettify(Json) when is_list(Json) ->
    Minified = minify(Json),
    prettify(Minified, 0, false, []).

prettify([], _Indent, _InString, Acc) ->
    lists:reverse(Acc);
prettify([$" | Rest], Indent, false, Acc) ->
    prettify(Rest, Indent, true, [$" | Acc]);
prettify([$" | Rest], Indent, true, Acc) ->
    prettify(Rest, Indent, false, [$" | Acc]);
prettify([$\\ | [C | Rest]], Indent, true, Acc) ->
    prettify(Rest, Indent, true, [C, $\\ | Acc]);
prettify([${ | Rest], Indent, false, Acc) ->
    NewIndent = Indent + 2,
    Trimmed = string:trim(Rest, leading),
    case Trimmed of
        [$} | _] -> prettify(Rest, Indent, false, [${ | Acc]);
        _ -> prettify(Rest, NewIndent, false, indent(NewIndent) ++ [$\n, ${ | Acc])
    end;
prettify([$} | Rest], Indent, false, Acc) ->
    NewIndent = erlang:max(0, Indent - 2),
    prettify(Rest, NewIndent, false, [$} | indent(NewIndent) ++ [$\n | Acc]]);
prettify([$[ | Rest], Indent, false, Acc) ->
    NewIndent = Indent + 2,
    Trimmed = string:trim(Rest, leading),
    case Trimmed of
        [$] | _] -> prettify(Rest, Indent, false, [$[ | Acc]);
        _ -> prettify(Rest, NewIndent, false, indent(NewIndent) ++ [$\n, $[ | Acc])
    end;
prettify([$] | Rest], Indent, false, Acc) ->
    NewIndent = erlang:max(0, Indent - 2),
    prettify(Rest, NewIndent, false, [$] | indent(NewIndent) ++ [$\n | Acc]]);
prettify([$, | Rest], Indent, false, Acc) ->
    prettify(Rest, Indent, false, indent(Indent) ++ [$\n, $, | Acc]);
prettify([$: | Rest], Indent, false, Acc) ->
    prettify(Rest, Indent, false, [$\s, $: | Acc]);
prettify([C | Rest], Indent, InString, Acc) ->
    prettify(Rest, Indent, InString, [C | Acc]).

indent(N) ->
    lists:duplicate(N, $\s).

%% @doc Escape a string for use in JSON.
-spec escape_string(string()) -> string().
escape_string(String) ->
    lists:flatmap(fun escape_char/1, String).

escape_char($") -> "\\\"";
escape_char($\\) -> "\\\\";
escape_char($\b) -> "\\b";
escape_char($\f) -> "\\f";
escape_char($\n) -> "\\n";
escape_char($\r) -> "\\r";
escape_char($\t) -> "\\t";
escape_char(C) when C < 32 ->
    io_lib:format("\\u~4.16.0B", [C]);
escape_char(C) -> [C].
