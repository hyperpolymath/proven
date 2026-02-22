%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeMath - Thin NIF wrapper for overflow-checked arithmetic.
%%
%% This module is a THIN FFI WRAPPER. All computation is performed in
%% formally verified Idris 2 code via the Zig FFI bridge (libproven).
%% No logic is reimplemented in Erlang -- only data marshaling.

-module(proven_math).

-export([
    safe_add/2,
    safe_sub/2,
    safe_mul/2,
    safe_div/2,
    safe_mod/2,
    safe_abs/1,
    clamp/3,
    safe_pow/2
]).

%% @doc Safe addition with overflow checking.
%% Delegates to proven:math_add_checked/2 NIF.
%% Returns {ok, Result} or {error, overflow}.
-spec safe_add(integer(), integer()) -> {ok, integer()} | {error, overflow}.
safe_add(A, B) ->
    proven:math_add_checked(A, B).

%% @doc Safe subtraction with overflow checking.
%% Delegates to proven:math_sub_checked/2 NIF.
%% Returns {ok, Result} or {error, overflow}.
-spec safe_sub(integer(), integer()) -> {ok, integer()} | {error, overflow}.
safe_sub(A, B) ->
    proven:math_sub_checked(A, B).

%% @doc Safe multiplication with overflow checking.
%% Delegates to proven:math_mul_checked/2 NIF.
%% Returns {ok, Result} or {error, overflow}.
-spec safe_mul(integer(), integer()) -> {ok, integer()} | {error, overflow}.
safe_mul(A, B) ->
    proven:math_mul_checked(A, B).

%% @doc Safe division with zero check.
%% Delegates to proven:math_div/2 NIF.
%% Returns {ok, Result} or {error, division_by_zero | overflow}.
-spec safe_div(integer(), integer()) -> {ok, integer()} | {error, division_by_zero | overflow}.
safe_div(A, B) ->
    proven:math_div(A, B).

%% @doc Safe modulo with zero check.
%% Delegates to proven:math_mod/2 NIF.
%% Returns {ok, Result} or {error, division_by_zero}.
-spec safe_mod(integer(), integer()) -> {ok, integer()} | {error, division_by_zero}.
safe_mod(A, B) ->
    proven:math_mod(A, B).

%% @doc Safe absolute value with overflow check.
%% Delegates to proven:math_abs_safe/1 NIF.
%% Returns {ok, Result} or {error, overflow}.
-spec safe_abs(integer()) -> {ok, integer()} | {error, overflow}.
safe_abs(A) ->
    proven:math_abs_safe(A).

%% @doc Clamp a value to a range [Lo, Hi].
%% Delegates to proven:math_clamp/3 NIF.
-spec clamp(number(), number(), number()) -> number().
clamp(Lo, Hi, Value) ->
    proven:math_clamp(Lo, Hi, Value).

%% @doc Safe exponentiation with overflow check.
%% Delegates to proven:math_pow_checked/2 NIF.
%% Returns {ok, Result} or {error, overflow}.
-spec safe_pow(integer(), integer()) -> {ok, integer()} | {error, overflow}.
safe_pow(Base, Exp) ->
    proven:math_pow_checked(Base, Exp).
