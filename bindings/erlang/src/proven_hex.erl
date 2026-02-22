%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeHex - Thin NIF wrapper for hexadecimal encoding/decoding.
%%
%% This module is a THIN FFI WRAPPER. All computation is performed in
%% formally verified Idris 2 code via the Zig FFI bridge (libproven).
%% No logic is reimplemented in Erlang -- only data marshaling.

-module(proven_hex).

-export([
    encode/1,
    encode_upper/1,
    decode/1,
    free/1
]).

%% @doc Encode binary to lowercase hexadecimal string.
%% Delegates to proven:hex_encode/2 NIF with Uppercase=false.
-spec encode(binary() | list()) -> binary().
encode(Bin) when is_binary(Bin) ->
    proven:hex_encode(Bin, false);
encode(Str) when is_list(Str) ->
    proven:hex_encode(list_to_binary(Str), false).

%% @doc Encode binary to uppercase hexadecimal string.
%% Delegates to proven:hex_encode/2 NIF with Uppercase=true.
-spec encode_upper(binary() | list()) -> binary().
encode_upper(Bin) when is_binary(Bin) ->
    proven:hex_encode(Bin, true);
encode_upper(Str) when is_list(Str) ->
    proven:hex_encode(list_to_binary(Str), true).

%% @doc Decode hexadecimal string to binary.
%% Delegates to proven:hex_decode/1 NIF.
-spec decode(binary() | string()) -> {ok, binary()} | {error, term()}.
decode(Bin) when is_binary(Bin) ->
    proven:hex_decode(Bin);
decode(Str) when is_list(Str) ->
    proven:hex_decode(list_to_binary(Str)).

%% @doc Free a NIF-allocated hex resource.
%% Delegates to proven:hex_free/1 NIF.
-spec free(reference()) -> ok.
free(Ref) ->
    proven:hex_free(Ref).
