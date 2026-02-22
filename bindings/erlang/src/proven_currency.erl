%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeCurrency - Thin NIF wrapper for currency operations.
%%
%% This module is a THIN FFI WRAPPER. All computation is performed in
%% formally verified Idris 2 code via the Zig FFI bridge (libproven).
%% No logic is reimplemented in Erlang -- only data marshaling.

-module(proven_currency).

-export([
    parse/1,
    format/3
]).

%% @doc Parse a currency string into a structured representation.
%% Delegates to proven:currency_parse/1 NIF.
-spec parse(binary() | string()) -> {ok, term()} | {error, term()}.
parse(Bin) when is_binary(Bin) ->
    proven:currency_parse(Bin);
parse(Str) when is_list(Str) ->
    proven:currency_parse(list_to_binary(Str)).

%% @doc Format a monetary amount with currency code and decimal places.
%% Delegates to proven:currency_format/3 NIF.
%% AmountMinor is the amount in minor units (e.g. cents).
%% Code is the ISO 4217 currency code as a binary (e.g. <<"USD">>).
%% DecimalPlaces is the number of decimal places for the currency.
-spec format(integer(), binary() | string(), non_neg_integer()) -> binary().
format(AmountMinor, Code, DecimalPlaces) when is_integer(AmountMinor),
                                               is_binary(Code),
                                               is_integer(DecimalPlaces) ->
    proven:currency_format(AmountMinor, Code, DecimalPlaces);
format(AmountMinor, Code, DecimalPlaces) when is_integer(AmountMinor),
                                               is_list(Code),
                                               is_integer(DecimalPlaces) ->
    proven:currency_format(AmountMinor, list_to_binary(Code), DecimalPlaces).
