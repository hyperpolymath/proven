%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeString - Thin NIF wrapper for XSS prevention.
%%
%% This module is a THIN FFI WRAPPER. All computation is performed in
%% formally verified Idris 2 code via the Zig FFI bridge (libproven).
%% No logic is reimplemented in Erlang -- only data marshaling.

-module(proven_string).

-export([
    is_valid_utf8/1,
    escape_html/1,
    escape_sql/1,
    escape_js/1
]).

%% @doc Check if a binary contains valid UTF-8.
%% Delegates to proven:string_is_valid_utf8/1 NIF.
-spec is_valid_utf8(binary()) -> boolean().
is_valid_utf8(Bin) when is_binary(Bin) ->
    proven:string_is_valid_utf8(Bin);
is_valid_utf8(Str) when is_list(Str) ->
    proven:string_is_valid_utf8(list_to_binary(Str)).

%% @doc Escape HTML special characters.
%% Delegates to proven:string_escape_html/1 NIF.
-spec escape_html(binary() | string()) -> binary().
escape_html(Bin) when is_binary(Bin) ->
    proven:string_escape_html(Bin);
escape_html(Str) when is_list(Str) ->
    proven:string_escape_html(list_to_binary(Str)).

%% @doc Escape SQL single quotes by doubling them.
%% Delegates to proven:string_escape_sql/1 NIF.
-spec escape_sql(binary() | string()) -> binary().
escape_sql(Bin) when is_binary(Bin) ->
    proven:string_escape_sql(Bin);
escape_sql(Str) when is_list(Str) ->
    proven:string_escape_sql(list_to_binary(Str)).

%% @doc Escape JavaScript special characters.
%% Delegates to proven:string_escape_js/1 NIF.
-spec escape_js(binary() | string()) -> binary().
escape_js(Bin) when is_binary(Bin) ->
    proven:string_escape_js(Bin);
escape_js(Str) when is_list(Str) ->
    proven:string_escape_js(list_to_binary(Str)).
