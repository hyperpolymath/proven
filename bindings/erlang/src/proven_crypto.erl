%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeCrypto - Thin NIF wrapper for cryptographic utilities.
%%
%% This module is a THIN FFI WRAPPER. All computation is performed in
%% formally verified Idris 2 code via the Zig FFI bridge (libproven).
%% No logic is reimplemented in Erlang -- only data marshaling.

-module(proven_crypto).

-export([
    constant_time_equals/2,
    random_bytes/1
]).

%% @doc Compare two binaries/strings in constant time.
%% Prevents timing attacks by always comparing all bytes.
%% Delegates to proven:crypto_constant_time_eq/2 NIF.
-spec constant_time_equals(binary() | string(), binary() | string()) -> boolean().
constant_time_equals(A, B) when is_list(A) ->
    constant_time_equals(list_to_binary(A), B);
constant_time_equals(A, B) when is_list(B) ->
    constant_time_equals(A, list_to_binary(B));
constant_time_equals(A, B) when is_binary(A), is_binary(B) ->
    proven:crypto_constant_time_eq(A, B).

%% @doc Generate cryptographically secure random bytes.
%% Delegates to proven:crypto_random_bytes/1 NIF.
-spec random_bytes(non_neg_integer()) -> binary().
random_bytes(Len) when is_integer(Len), Len >= 0 ->
    proven:crypto_random_bytes(Len).
