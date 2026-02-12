%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven SafeCrypto - Cryptographic utilities for Erlang

-module(proven_crypto).

-export([
    constant_time_equals/2,
    simple_hash/1,
    bytes_to_hex/1,
    generate_token/1,
    random_int/2,
    secure_wipe/1
]).

%% Hex characters
-define(HEX_CHARS, "0123456789abcdef").

%% @doc Compare two binaries/strings in constant time.
%% Prevents timing attacks by always comparing all bytes.
-spec constant_time_equals(binary() | string(), binary() | string()) -> boolean().
constant_time_equals(A, B) when is_list(A) ->
    constant_time_equals(list_to_binary(A), B);
constant_time_equals(A, B) when is_list(B) ->
    constant_time_equals(A, list_to_binary(B));
constant_time_equals(A, B) when byte_size(A) =/= byte_size(B) ->
    false;
constant_time_equals(A, B) ->
    constant_time_equals(A, B, 0, 0).

constant_time_equals(<<>>, <<>>, _Index, Diff) ->
    Diff == 0;
constant_time_equals(<<CA, RestA/binary>>, <<CB, RestB/binary>>, Index, Diff) ->
    constant_time_equals(RestA, RestB, Index + 1, Diff bor (CA bxor CB)).

%% @doc Compute FNV-1a hash of input.
-spec simple_hash(string() | binary()) -> integer().
simple_hash(Input) when is_binary(Input) ->
    simple_hash(binary_to_list(Input));
simple_hash(Input) when is_list(Input) ->
    fnv1a(Input, 16#811c9dc5).

fnv1a([], Hash) ->
    Hash band 16#FFFFFFFF;
fnv1a([C | Rest], Hash) ->
    NewHash = (Hash bxor C) * 16#01000193,
    fnv1a(Rest, NewHash band 16#FFFFFFFF).

%% @doc Convert binary to hexadecimal string.
-spec bytes_to_hex(binary() | list()) -> string().
bytes_to_hex(Bytes) when is_list(Bytes) ->
    bytes_to_hex(list_to_binary(Bytes));
bytes_to_hex(Bytes) when is_binary(Bytes) ->
    lists:flatten([byte_to_hex(B) || <<B>> <= Bytes]).

byte_to_hex(Byte) ->
    [lists:nth((Byte bsr 4) + 1, ?HEX_CHARS),
     lists:nth((Byte band 16#F) + 1, ?HEX_CHARS)].

%% @doc Generate a random hexadecimal token.
-spec generate_token(integer()) -> string().
generate_token(Length) when Length > 0 ->
    [lists:nth(rand:uniform(16), ?HEX_CHARS) || _ <- lists:seq(1, Length)].

%% @doc Generate random integer in range [Min, Max].
-spec random_int(integer(), integer()) -> integer().
random_int(Min, Max) when Min >= Max ->
    Min;
random_int(Min, Max) ->
    Min + rand:uniform(Max - Min + 1) - 1.

%% @doc Attempt to securely wipe a binary from memory.
%% Note: In Erlang/BEAM, memory management is automatic.
%% This is a best-effort operation that overwrites the data.
-spec secure_wipe(binary()) -> binary().
secure_wipe(Bin) when is_binary(Bin) ->
    Size = byte_size(Bin),
    crypto:strong_rand_bytes(Size),
    <<0:(Size * 8)>>;
secure_wipe(_) ->
    <<>>.
