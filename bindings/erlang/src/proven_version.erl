%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeVersion - Thin NIF wrapper for semantic versioning.
%%
%% This module is a THIN FFI WRAPPER. All computation is performed in
%% formally verified Idris 2 code via the Zig FFI bridge (libproven).
%% No logic is reimplemented in Erlang -- only data marshaling.

-module(proven_version).

-export([
    parse/1,
    compare/2,
    free/1
]).

%% @doc Parse a semantic version string (e.g. "1.2.3", "v2.0.0-beta.1").
%% Delegates to proven:version_parse/1 NIF.
%% Returns an opaque reference that must be freed with free/1 when done.
-spec parse(binary() | string()) -> {ok, reference()} | {error, term()}.
parse(Bin) when is_binary(Bin) ->
    proven:version_parse(Bin);
parse(Str) when is_list(Str) ->
    proven:version_parse(list_to_binary(Str)).

%% @doc Compare two parsed version references.
%% Delegates to proven:version_compare/2 NIF.
%% Returns lt, eq, or gt.
-spec compare(reference(), reference()) -> lt | eq | gt.
compare(VersionA, VersionB) ->
    proven:version_compare(VersionA, VersionB).

%% @doc Free a parsed version reference returned by parse/1.
%% Delegates to proven:version_free/1 NIF.
-spec free(reference()) -> ok.
free(Ref) ->
    proven:version_free(Ref).
