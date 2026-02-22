%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeDateTime - Thin NIF wrapper for date and time validation.
%%
%% This module is a THIN FFI WRAPPER. All computation is performed in
%% formally verified Idris 2 code via the Zig FFI bridge (libproven).
%% No logic is reimplemented in Erlang -- only data marshaling.

-module(proven_datetime).

-export([
    parse/1,
    format_iso8601/1,
    is_leap_year/1,
    days_in_month/2
]).

%% @doc Parse an ISO 8601 datetime string.
%% Delegates to proven:datetime_parse/1 NIF.
-spec parse(binary() | string()) -> {ok, term()} | {error, term()}.
parse(Bin) when is_binary(Bin) ->
    proven:datetime_parse(Bin);
parse(Str) when is_list(Str) ->
    proven:datetime_parse(list_to_binary(Str)).

%% @doc Format a datetime map/struct as ISO 8601.
%% Delegates to proven:datetime_format_iso8601/1 NIF.
%% The DatetimeMap should match the structure expected by the NIF.
-spec format_iso8601(map()) -> binary().
format_iso8601(DatetimeMap) when is_map(DatetimeMap) ->
    proven:datetime_format_iso8601(DatetimeMap).

%% @doc Check if a year is a leap year.
%% Delegates to proven:datetime_is_leap_year/1 NIF.
-spec is_leap_year(integer()) -> boolean().
is_leap_year(Year) when is_integer(Year) ->
    proven:datetime_is_leap_year(Year).

%% @doc Get the number of days in a given month of a given year.
%% Delegates to proven:datetime_days_in_month/2 NIF.
-spec days_in_month(integer(), integer()) -> integer().
days_in_month(Year, Month) when is_integer(Year), is_integer(Month) ->
    proven:datetime_days_in_month(Year, Month).
