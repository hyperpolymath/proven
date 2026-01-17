%% SPDX-License-Identifier: PMPL-1.0
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven SafeHex - Hexadecimal encoding/decoding for Erlang

-module(proven_hex).

-export([
    encode/1,
    encode_upper/1,
    decode/1,
    is_valid/1,
    constant_time_equal/2,
    from_integer/2,
    to_integer/1,
    xor_hex/2,
    length_bytes/1
]).

%% Hex character lookup tables
-define(HEX_LOWER, "0123456789abcdef").
-define(HEX_UPPER, "0123456789ABCDEF").

%% @doc Encode binary or string to lowercase hexadecimal string.
-spec encode(binary() | list()) -> string().
encode(Input) when is_binary(Input) ->
    encode_binary(Input, ?HEX_LOWER);
encode(Input) when is_list(Input) ->
    encode_binary(list_to_binary(Input), ?HEX_LOWER).

%% @doc Encode binary or string to uppercase hexadecimal string.
-spec encode_upper(binary() | list()) -> string().
encode_upper(Input) when is_binary(Input) ->
    encode_binary(Input, ?HEX_UPPER);
encode_upper(Input) when is_list(Input) ->
    encode_binary(list_to_binary(Input), ?HEX_UPPER).

encode_binary(<<>>, _HexChars) ->
    [];
encode_binary(<<Byte, Rest/binary>>, HexChars) ->
    HighNibble = (Byte bsr 4) band 16#F,
    LowNibble = Byte band 16#F,
    [lists:nth(HighNibble + 1, HexChars),
     lists:nth(LowNibble + 1, HexChars) | encode_binary(Rest, HexChars)].

%% @doc Decode hexadecimal string to binary.
-spec decode(string() | binary()) -> {ok, binary()} | {error, invalid_hex}.
decode(Input) when is_binary(Input) ->
    decode(binary_to_list(Input));
decode(Input) when is_list(Input) ->
    %% Remove any whitespace and validate
    Cleaned = [C || C <- Input, C =/= $\s, C =/= $\n, C =/= $\r, C =/= $\t],
    case length(Cleaned) rem 2 of
        0 ->
            decode_pairs(Cleaned, []);
        _ ->
            {error, invalid_hex}
    end.

decode_pairs([], Acc) ->
    {ok, list_to_binary(lists:reverse(Acc))};
decode_pairs([H1, H2 | Rest], Acc) ->
    case {hex_value(H1), hex_value(H2)} of
        {{ok, V1}, {ok, V2}} ->
            Byte = (V1 bsl 4) bor V2,
            decode_pairs(Rest, [Byte | Acc]);
        _ ->
            {error, invalid_hex}
    end.

hex_value(C) when C >= $0, C =< $9 -> {ok, C - $0};
hex_value(C) when C >= $a, C =< $f -> {ok, C - $a + 10};
hex_value(C) when C >= $A, C =< $F -> {ok, C - $A + 10};
hex_value(_) -> error.

%% @doc Check if a string is valid hexadecimal.
-spec is_valid(string() | binary()) -> boolean().
is_valid(Input) when is_binary(Input) ->
    is_valid(binary_to_list(Input));
is_valid(Input) when is_list(Input) ->
    Cleaned = [C || C <- Input, C =/= $\s, C =/= $\n, C =/= $\r, C =/= $\t],
    length(Cleaned) rem 2 == 0 andalso
    lists:all(fun is_hex_char/1, Cleaned).

is_hex_char(C) when C >= $0, C =< $9 -> true;
is_hex_char(C) when C >= $a, C =< $f -> true;
is_hex_char(C) when C >= $A, C =< $F -> true;
is_hex_char(_) -> false.

%% @doc Compare two hex strings in constant time.
%% Prevents timing attacks by always comparing all characters.
-spec constant_time_equal(string() | binary(), string() | binary()) -> boolean().
constant_time_equal(A, B) when is_binary(A) ->
    constant_time_equal(binary_to_list(A), B);
constant_time_equal(A, B) when is_binary(B) ->
    constant_time_equal(A, binary_to_list(B));
constant_time_equal(A, B) when is_list(A), is_list(B) ->
    %% Normalize to lowercase for comparison
    NormA = string:lowercase([C || C <- A, C =/= $\s]),
    NormB = string:lowercase([C || C <- B, C =/= $\s]),
    constant_time_equal_impl(NormA, NormB, 0).

constant_time_equal_impl([], [], Diff) ->
    Diff == 0;
constant_time_equal_impl([_|_], [], _Diff) ->
    false;
constant_time_equal_impl([], [_|_], _Diff) ->
    false;
constant_time_equal_impl([CA | RestA], [CB | RestB], Diff) ->
    %% XOR the characters and OR into the difference accumulator
    NewDiff = Diff bor (CA bxor CB),
    constant_time_equal_impl(RestA, RestB, NewDiff).

%% @doc Convert an integer to a hex string with specified minimum width.
%% Pads with leading zeros if necessary.
-spec from_integer(non_neg_integer(), pos_integer()) -> string().
from_integer(Value, MinWidth) when is_integer(Value), Value >= 0, is_integer(MinWidth), MinWidth > 0 ->
    Hex = integer_to_list(Value, 16),
    LowerHex = string:lowercase(Hex),
    pad_left(LowerHex, MinWidth, $0).

pad_left(Str, Width, PadChar) when length(Str) >= Width ->
    Str;
pad_left(Str, Width, PadChar) ->
    pad_left([PadChar | Str], Width, PadChar).

%% @doc Convert a hex string to an integer.
-spec to_integer(string() | binary()) -> {ok, non_neg_integer()} | {error, invalid_hex}.
to_integer(Input) when is_binary(Input) ->
    to_integer(binary_to_list(Input));
to_integer(Input) when is_list(Input) ->
    Cleaned = [C || C <- Input, C =/= $\s],
    case is_valid(Cleaned) of
        true when Cleaned =/= [] ->
            try
                {ok, list_to_integer(Cleaned, 16)}
            catch
                _:_ -> {error, invalid_hex}
            end;
        true ->
            {ok, 0};
        false ->
            {error, invalid_hex}
    end.

%% @doc XOR two hex strings together.
%% Both strings must be the same length.
-spec xor_hex(string() | binary(), string() | binary()) -> {ok, string()} | {error, term()}.
xor_hex(A, B) when is_binary(A) ->
    xor_hex(binary_to_list(A), B);
xor_hex(A, B) when is_binary(B) ->
    xor_hex(A, binary_to_list(B));
xor_hex(A, B) when is_list(A), is_list(B) ->
    case {decode(A), decode(B)} of
        {{ok, BinA}, {ok, BinB}} when byte_size(BinA) == byte_size(BinB) ->
            XorResult = crypto:exor(BinA, BinB),
            {ok, encode(XorResult)};
        {{ok, _}, {ok, _}} ->
            {error, length_mismatch};
        {{error, _} = Error, _} ->
            Error;
        {_, {error, _} = Error} ->
            Error
    end.

%% @doc Get the byte length that a hex string represents.
-spec length_bytes(string() | binary()) -> {ok, non_neg_integer()} | {error, invalid_hex}.
length_bytes(Input) when is_binary(Input) ->
    length_bytes(binary_to_list(Input));
length_bytes(Input) when is_list(Input) ->
    Cleaned = [C || C <- Input, C =/= $\s, C =/= $\n, C =/= $\r, C =/= $\t],
    case is_valid(Cleaned) of
        true -> {ok, length(Cleaned) div 2};
        false -> {error, invalid_hex}
    end.
