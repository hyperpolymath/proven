%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeJSON - Thin NIF wrapper for JSON validation.
%%
%% This module is a THIN FFI WRAPPER. All computation is performed in
%% formally verified Idris 2 code via the Zig FFI bridge (libproven).
%% No logic is reimplemented in Erlang -- only data marshaling.

-module(proven_json).

-export([
    is_valid/1,
    get_type/1
]).

%% @doc Check if a string is valid JSON syntax.
%% Delegates to proven:json_is_valid/1 NIF.
-spec is_valid(binary() | string()) -> boolean().
is_valid(Bin) when is_binary(Bin) ->
    proven:json_is_valid(Bin);
is_valid(Str) when is_list(Str) ->
    proven:json_is_valid(list_to_binary(Str)).

%% @doc Get the top-level JSON value type (object, array, string, number, etc.).
%% Delegates to proven:json_get_type/1 NIF.
-spec get_type(binary() | string()) -> atom() | {error, term()}.
get_type(Bin) when is_binary(Bin) ->
    proven:json_get_type(Bin);
get_type(Str) when is_list(Str) ->
    proven:json_get_type(list_to_binary(Str)).
