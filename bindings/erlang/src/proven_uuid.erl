%% SPDX-License-Identifier: PMPL-1.0
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven SafeUUID - UUID validation and generation for Erlang

-module(proven_uuid).

-export([
    new/0,
    parse/1,
    format/1,
    is_valid/1,
    to_binary/1,
    from_binary/1,
    version/1,
    variant/1,
    nil/0,
    is_nil/1
]).

-export_type([uuid/0]).

%% UUID record: stores 128 bits as two 64-bit integers
-record(uuid, {
    high :: non_neg_integer(),
    low :: non_neg_integer()
}).

-type uuid() :: #uuid{}.

%% Hex character lookup
-define(HEX_CHARS, "0123456789abcdef").

%% @doc Generate a new random UUID (version 4).
-spec new() -> uuid().
new() ->
    RandomBytes = crypto:strong_rand_bytes(16),
    <<High0:64, Low0:64>> = RandomBytes,
    %% Set version to 4 (bits 12-15 of time_hi_and_version)
    High1 = (High0 band 16#FFFFFFFFFFFF0FFF) bor 16#0000000000004000,
    %% Set variant to RFC 4122 (bits 6-7 of clock_seq_hi_and_reserved)
    Low1 = (Low0 band 16#3FFFFFFFFFFFFFFF) bor 16#8000000000000000,
    #uuid{high = High1, low = Low1}.

%% @doc Parse a UUID string into a uuid record.
%% Accepts formats: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
%%                  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
%%                  {xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}
-spec parse(string() | binary()) -> {ok, uuid()} | {error, invalid_format}.
parse(Input) when is_binary(Input) ->
    parse(binary_to_list(Input));
parse(Input) when is_list(Input) ->
    Normalized = normalize_uuid_string(Input),
    case Normalized of
        {ok, HexString} ->
            parse_hex_string(HexString);
        error ->
            {error, invalid_format}
    end.

normalize_uuid_string([${ | Rest]) ->
    case lists:reverse(Rest) of
        [$} | RevInner] ->
            normalize_uuid_string(lists:reverse(RevInner));
        _ ->
            error
    end;
normalize_uuid_string(Input) ->
    Lower = string:lowercase(Input),
    case strip_hyphens(Lower) of
        {ok, Stripped} when length(Stripped) == 32 ->
            case lists:all(fun is_hex_char/1, Stripped) of
                true -> {ok, Stripped};
                false -> error
            end;
        _ ->
            error
    end.

strip_hyphens(Input) ->
    Stripped = [C || C <- Input, C =/= $-],
    {ok, Stripped}.

is_hex_char(C) when C >= $0, C =< $9 -> true;
is_hex_char(C) when C >= $a, C =< $f -> true;
is_hex_char(_) -> false.

parse_hex_string(HexString) when length(HexString) == 32 ->
    try
        High = list_to_integer(lists:sublist(HexString, 1, 16), 16),
        Low = list_to_integer(lists:sublist(HexString, 17, 16), 16),
        {ok, #uuid{high = High, low = Low}}
    catch
        _:_ -> {error, invalid_format}
    end;
parse_hex_string(_) ->
    {error, invalid_format}.

%% @doc Format a UUID as a hyphenated lowercase string.
-spec format(uuid()) -> string().
format(#uuid{high = High, low = Low}) ->
    HexHigh = integer_to_hex(High, 16),
    HexLow = integer_to_hex(Low, 16),
    format_with_hyphens(HexHigh ++ HexLow).

integer_to_hex(Value, Digits) ->
    Hex = integer_to_list(Value, 16),
    LowerHex = string:lowercase(Hex),
    pad_left(LowerHex, Digits, $0).

pad_left(Str, Len, PadChar) when length(Str) >= Len ->
    Str;
pad_left(Str, Len, PadChar) ->
    pad_left([PadChar | Str], Len, PadChar).

format_with_hyphens(Hex) when length(Hex) == 32 ->
    [A, B, C, D, E, F, G, H,
     I, J, K, L,
     M, N, O, P,
     Q, R, S, T,
     U, V, W, X, Y, Z, AA, AB, AC, AD, AE, AF] = Hex,
    [A, B, C, D, E, F, G, H, $-,
     I, J, K, L, $-,
     M, N, O, P, $-,
     Q, R, S, T, $-,
     U, V, W, X, Y, Z, AA, AB, AC, AD, AE, AF].

%% @doc Check if a string is a valid UUID.
-spec is_valid(string() | binary()) -> boolean().
is_valid(Input) ->
    case parse(Input) of
        {ok, _} -> true;
        {error, _} -> false
    end.

%% @doc Convert a UUID to a 16-byte binary.
-spec to_binary(uuid()) -> binary().
to_binary(#uuid{high = High, low = Low}) ->
    <<High:64, Low:64>>.

%% @doc Create a UUID from a 16-byte binary.
-spec from_binary(binary()) -> {ok, uuid()} | {error, invalid_size}.
from_binary(<<High:64, Low:64>>) ->
    {ok, #uuid{high = High, low = Low}};
from_binary(_) ->
    {error, invalid_size}.

%% @doc Get the version number of a UUID.
-spec version(uuid()) -> 1..5.
version(#uuid{high = High}) ->
    (High bsr 12) band 16#F.

%% @doc Get the variant of a UUID.
%% Returns: ncs | rfc4122 | microsoft | future
-spec variant(uuid()) -> ncs | rfc4122 | microsoft | future.
variant(#uuid{low = Low}) ->
    VariantBits = (Low bsr 62) band 16#3,
    case VariantBits of
        0 -> ncs;
        1 -> ncs;
        2 -> rfc4122;
        3 ->
            case (Low bsr 61) band 16#7 of
                6 -> microsoft;
                7 -> future;
                _ -> rfc4122
            end
    end.

%% @doc Return the nil UUID (all zeros).
-spec nil() -> uuid().
nil() ->
    #uuid{high = 0, low = 0}.

%% @doc Check if a UUID is the nil UUID.
-spec is_nil(uuid()) -> boolean().
is_nil(#uuid{high = 0, low = 0}) -> true;
is_nil(_) -> false.
