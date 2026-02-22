%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeEmail - Thin NIF wrapper for email validation.
%%
%% This module is a THIN FFI WRAPPER. All computation is performed in
%% formally verified Idris 2 code via the Zig FFI bridge (libproven).
%% No logic is reimplemented in Erlang -- only data marshaling.

-module(proven_email).

-export([
    is_valid/1
]).

%% @doc Check if an email address is valid.
%% Delegates to proven:email_is_valid/1 NIF.
-spec is_valid(binary() | string()) -> boolean().
is_valid(Bin) when is_binary(Bin) ->
    proven:email_is_valid(Bin);
is_valid(Str) when is_list(Str) ->
    proven:email_is_valid(list_to_binary(Str)).
