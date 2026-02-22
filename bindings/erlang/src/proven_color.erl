%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeColor - Thin NIF wrapper for color operations.
%%
%% This module is a THIN FFI WRAPPER. All computation is performed in
%% formally verified Idris 2 code via the Zig FFI bridge (libproven).
%% No logic is reimplemented in Erlang -- only data marshaling.

-module(proven_color).

-export([
    parse_hex/1,
    rgb_to_hsl/1,
    to_hex/1
]).

%% @doc Parse a hex color string (e.g. "#FF0080", "ff0080", "#abc").
%% Delegates to proven:color_parse_hex/1 NIF.
-spec parse_hex(binary() | string()) -> {ok, term()} | {error, term()}.
parse_hex(Bin) when is_binary(Bin) ->
    proven:color_parse_hex(Bin);
parse_hex(Str) when is_list(Str) ->
    proven:color_parse_hex(list_to_binary(Str)).

%% @doc Convert an RGB color value to HSL.
%% Delegates to proven:color_rgb_to_hsl/1 NIF.
%% Rgb should be the structured value returned by parse_hex/1 or similar.
-spec rgb_to_hsl(term()) -> term().
rgb_to_hsl(Rgb) ->
    proven:color_rgb_to_hsl(Rgb).

%% @doc Convert an RGB color value to a hex string.
%% Delegates to proven:color_to_hex/1 NIF.
%% Rgb should be the structured value returned by parse_hex/1 or similar.
-spec to_hex(term()) -> binary().
to_hex(Rgb) ->
    proven:color_to_hex(Rgb).
