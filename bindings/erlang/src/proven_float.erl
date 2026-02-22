%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeFloat - Thin NIF wrapper for safe floating-point operations.
%%
%% This module is a THIN FFI WRAPPER. All computation is performed in
%% formally verified Idris 2 code via the Zig FFI bridge (libproven).
%% No logic is reimplemented in Erlang -- only data marshaling.

-module(proven_float).

-export([
    safe_div/2,
    is_finite/1,
    is_nan/1,
    safe_sqrt/1,
    safe_ln/1
]).

%% @doc Safe floating-point division with zero and overflow checking.
%% Delegates to proven:float_div/2 NIF.
-spec safe_div(number(), number()) -> {ok, float()} | {error, term()}.
safe_div(A, B) when is_number(A), is_number(B) ->
    proven:float_div(A, B).

%% @doc Check if a float is finite (not NaN or infinity).
%% Delegates to proven:float_is_finite/1 NIF.
-spec is_finite(number()) -> boolean().
is_finite(X) when is_number(X) ->
    proven:float_is_finite(X).

%% @doc Check if a value is NaN.
%% Delegates to proven:float_is_nan/1 NIF.
-spec is_nan(number()) -> boolean().
is_nan(X) when is_number(X) ->
    proven:float_is_nan(X).

%% @doc Safe square root (fails for negative numbers).
%% Delegates to proven:float_sqrt/1 NIF.
-spec safe_sqrt(number()) -> {ok, float()} | {error, term()}.
safe_sqrt(X) when is_number(X) ->
    proven:float_sqrt(X).

%% @doc Safe natural logarithm (fails for non-positive numbers).
%% Delegates to proven:float_ln/1 NIF.
-spec safe_ln(number()) -> {ok, float()} | {error, term()}.
safe_ln(X) when is_number(X) ->
    proven:float_ln(X).
