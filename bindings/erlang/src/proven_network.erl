%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeNetwork - Thin NIF wrapper for network validation.
%%
%% This module is a THIN FFI WRAPPER. All computation is performed in
%% formally verified Idris 2 code via the Zig FFI bridge (libproven).
%% No logic is reimplemented in Erlang -- only data marshaling.

-module(proven_network).

-export([
    parse_ipv4/1,
    is_private/1,
    is_loopback/1
]).

%% @doc Parse an IPv4 address string into octets.
%% Delegates to proven:network_parse_ipv4/1 NIF.
-spec parse_ipv4(binary() | string()) -> {ok, tuple()} | {error, term()}.
parse_ipv4(Bin) when is_binary(Bin) ->
    proven:network_parse_ipv4(Bin);
parse_ipv4(Str) when is_list(Str) ->
    proven:network_parse_ipv4(list_to_binary(Str)).

%% @doc Check if IPv4 octets represent a private address.
%% Delegates to proven:network_ipv4_is_private/1 NIF.
%% Expects the octets value returned by parse_ipv4/1.
-spec is_private(tuple()) -> boolean().
is_private(Octets) ->
    proven:network_ipv4_is_private(Octets).

%% @doc Check if IPv4 octets represent a loopback address (127.x.x.x).
%% Delegates to proven:network_ipv4_is_loopback/1 NIF.
%% Expects the octets value returned by parse_ipv4/1.
-spec is_loopback(tuple()) -> boolean().
is_loopback(Octets) ->
    proven:network_ipv4_is_loopback(Octets).
