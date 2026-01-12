%% SPDX-License-Identifier: PMPL-1.0
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven SafeMath - Overflow-checked arithmetic for Erlang

-module(proven_math).

-export([
    safe_add/2,
    safe_sub/2,
    safe_mul/2,
    safe_div/2,
    safe_mod/2,
    safe_abs/1,
    safe_negate/1,
    clamp/3,
    in_range/3,
    max_int64/0,
    min_int64/0
]).

%% Constants for 64-bit integers
-define(MAX_INT64, 9223372036854775807).
-define(MIN_INT64, -9223372036854775808).

%% @doc Returns the maximum 64-bit integer value.
-spec max_int64() -> integer().
max_int64() -> ?MAX_INT64.

%% @doc Returns the minimum 64-bit integer value.
-spec min_int64() -> integer().
min_int64() -> ?MIN_INT64.

%% @doc Safe addition with overflow checking.
%% Returns {ok, Result} or {error, overflow}.
-spec safe_add(integer(), integer()) -> {ok, integer()} | {error, overflow}.
safe_add(A, B) when B > 0, A > ?MAX_INT64 - B ->
    {error, overflow};
safe_add(A, B) when B < 0, A < ?MIN_INT64 - B ->
    {error, overflow};
safe_add(A, B) ->
    {ok, A + B}.

%% @doc Safe subtraction with overflow checking.
%% Returns {ok, Result} or {error, overflow}.
-spec safe_sub(integer(), integer()) -> {ok, integer()} | {error, overflow}.
safe_sub(A, B) when B < 0, A > ?MAX_INT64 + B ->
    {error, overflow};
safe_sub(A, B) when B > 0, A < ?MIN_INT64 + B ->
    {error, overflow};
safe_sub(A, B) ->
    {ok, A - B}.

%% @doc Safe multiplication with overflow checking.
%% Returns {ok, Result} or {error, overflow}.
-spec safe_mul(integer(), integer()) -> {ok, integer()} | {error, overflow}.
safe_mul(A, _B) when A == 0 ->
    {ok, 0};
safe_mul(_A, B) when B == 0 ->
    {ok, 0};
safe_mul(A, B) ->
    Result = A * B,
    case Result div A of
        B -> {ok, Result};
        _ -> {error, overflow}
    end.

%% @doc Safe division with zero check.
%% Returns {ok, Result} or {error, Reason}.
-spec safe_div(integer(), integer()) -> {ok, integer()} | {error, division_by_zero | overflow}.
safe_div(_A, 0) ->
    {error, division_by_zero};
safe_div(A, -1) when A == ?MIN_INT64 ->
    {error, overflow};
safe_div(A, B) ->
    {ok, A div B}.

%% @doc Safe modulo with zero check.
%% Returns {ok, Result} or {error, division_by_zero}.
-spec safe_mod(integer(), integer()) -> {ok, integer()} | {error, division_by_zero}.
safe_mod(_A, 0) ->
    {error, division_by_zero};
safe_mod(A, B) ->
    {ok, A rem B}.

%% @doc Safe absolute value with overflow check.
%% Returns {ok, Result} or {error, overflow}.
-spec safe_abs(integer()) -> {ok, integer()} | {error, overflow}.
safe_abs(A) when A == ?MIN_INT64 ->
    {error, overflow};
safe_abs(A) ->
    {ok, abs(A)}.

%% @doc Safe negation with overflow check.
%% Returns {ok, Result} or {error, overflow}.
-spec safe_negate(integer()) -> {ok, integer()} | {error, overflow}.
safe_negate(A) when A == ?MIN_INT64 ->
    {error, overflow};
safe_negate(A) ->
    {ok, -A}.

%% @doc Clamp a value to a range.
-spec clamp(number(), number(), number()) -> number().
clamp(Value, Min, _Max) when Value < Min ->
    Min;
clamp(Value, _Min, Max) when Value > Max ->
    Max;
clamp(Value, _Min, _Max) ->
    Value.

%% @doc Check if a value is within a range (inclusive).
-spec in_range(number(), number(), number()) -> boolean().
in_range(Value, Min, Max) ->
    Value >= Min andalso Value =< Max.
