%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeUUID - Thin NIF wrapper for UUID operations.
%%
%% This module is a THIN FFI WRAPPER. All computation is performed in
%% formally verified Idris 2 code via the Zig FFI bridge (libproven).
%% No logic is reimplemented in Erlang -- only data marshaling.

-module(proven_uuid).

-export([
    v4/0,
    to_string/1,
    parse/1,
    is_nil/1,
    version/1
]).

%% @doc Generate a new random UUID (version 4).
%% Delegates to proven:uuid_v4/0 NIF.
-spec v4() -> binary().
v4() ->
    proven:uuid_v4().

%% @doc Convert a UUID binary to its string representation.
%% Delegates to proven:uuid_to_string/1 NIF.
-spec to_string(binary()) -> binary().
to_string(UuidBin) when is_binary(UuidBin) ->
    proven:uuid_to_string(UuidBin).

%% @doc Parse a UUID string into a 16-byte binary.
%% Delegates to proven:uuid_parse/1 NIF.
-spec parse(binary() | string()) -> {ok, binary()} | {error, term()}.
parse(Bin) when is_binary(Bin) ->
    proven:uuid_parse(Bin);
parse(Str) when is_list(Str) ->
    proven:uuid_parse(list_to_binary(Str)).

%% @doc Check if a UUID is the nil UUID (all zeros).
%% Delegates to proven:uuid_is_nil/1 NIF.
-spec is_nil(binary()) -> boolean().
is_nil(UuidBin) when is_binary(UuidBin) ->
    proven:uuid_is_nil(UuidBin).

%% @doc Get the version number of a UUID.
%% Delegates to proven:uuid_version/1 NIF.
-spec version(binary()) -> integer().
version(UuidBin) when is_binary(UuidBin) ->
    proven:uuid_version(UuidBin).
