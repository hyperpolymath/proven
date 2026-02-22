%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeURL - Thin NIF wrapper for URL parsing and validation.
%%
%% This module is a THIN FFI WRAPPER. All computation is performed in
%% formally verified Idris 2 code via the Zig FFI bridge (libproven).
%% No logic is reimplemented in Erlang -- only data marshaling.

-module(proven_url).

-export([
    parse/1,
    free/1
]).

%% @doc Parse a URL string into a NIF-managed parsed URL reference.
%% Returns an opaque reference that must be freed with free/1 when done.
%% Delegates to proven:url_parse/1 NIF.
-spec parse(binary() | string()) -> {ok, reference()} | {error, term()}.
parse(Bin) when is_binary(Bin) ->
    proven:url_parse(Bin);
parse(Str) when is_list(Str) ->
    proven:url_parse(list_to_binary(Str)).

%% @doc Free a parsed URL reference returned by parse/1.
%% Delegates to proven:url_free/1 NIF.
-spec free(reference()) -> ok.
free(Ref) ->
    proven:url_free(Ref).
