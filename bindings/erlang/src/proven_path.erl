%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafePath - Thin NIF wrapper for directory traversal prevention.
%%
%% This module is a THIN FFI WRAPPER. All computation is performed in
%% formally verified Idris 2 code via the Zig FFI bridge (libproven).
%% No logic is reimplemented in Erlang -- only data marshaling.

-module(proven_path).

-export([
    has_traversal/1,
    sanitize_filename/1
]).

%% @doc Check if path contains traversal patterns (e.g. "..", encoded variants).
%% Delegates to proven:path_has_traversal/1 NIF.
-spec has_traversal(binary() | string()) -> boolean().
has_traversal(Bin) when is_binary(Bin) ->
    proven:path_has_traversal(Bin);
has_traversal(Str) when is_list(Str) ->
    proven:path_has_traversal(list_to_binary(Str)).

%% @doc Sanitize a filename by replacing dangerous characters.
%% Delegates to proven:path_sanitize_filename/1 NIF.
-spec sanitize_filename(binary() | string()) -> binary().
sanitize_filename(Bin) when is_binary(Bin) ->
    proven:path_sanitize_filename(Bin);
sanitize_filename(Str) when is_list(Str) ->
    proven:path_sanitize_filename(list_to_binary(Str)).
