%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafePhone - Thin NIF wrapper for phone number operations.
%%
%% This module is a THIN FFI WRAPPER. All computation is performed in
%% formally verified Idris 2 code via the Zig FFI bridge (libproven).
%% No logic is reimplemented in Erlang -- only data marshaling.

-module(proven_phone).

-export([
    parse/1,
    format_e164/2
]).

%% @doc Parse a phone number string.
%% Delegates to proven:phone_parse/1 NIF.
-spec parse(binary() | string()) -> {ok, term()} | {error, term()}.
parse(Bin) when is_binary(Bin) ->
    proven:phone_parse(Bin);
parse(Str) when is_list(Str) ->
    proven:phone_parse(list_to_binary(Str)).

%% @doc Format a phone number in E.164 format from country code and national number.
%% Delegates to proven:phone_format_e164/2 NIF.
-spec format_e164(integer(), binary() | string()) -> binary().
format_e164(CountryCode, NationalNumber) when is_integer(CountryCode),
                                               is_binary(NationalNumber) ->
    proven:phone_format_e164(CountryCode, NationalNumber);
format_e164(CountryCode, NationalNumber) when is_integer(CountryCode),
                                               is_list(NationalNumber) ->
    proven:phone_format_e164(CountryCode, list_to_binary(NationalNumber)).
