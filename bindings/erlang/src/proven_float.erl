%% SPDX-License-Identifier: PMPL-1.0
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven SafeFloat - Safe floating-point operations for Erlang

-module(proven_float).

-export([
    is_finite/1,
    is_nan/1,
    is_infinite/1,
    safe_add/2,
    safe_sub/2,
    safe_mul/2,
    safe_div/2,
    safe_sqrt/1,
    safe_pow/2,
    safe_log/1,
    safe_log10/1,
    approximately_equal/3,
    round_to/2,
    truncate_to/2,
    floor_to/2,
    ceiling_to/2,
    clamp/3,
    lerp/3,
    normalize/3,
    epsilon/0
]).

%% Machine epsilon for double precision
-define(EPSILON, 2.220446049250313e-16).

%% @doc Get machine epsilon.
-spec epsilon() -> float().
epsilon() -> ?EPSILON.

%% @doc Check if a float is finite (not NaN or infinity).
-spec is_finite(float()) -> boolean().
is_finite(X) ->
    not (is_nan(X) orelse is_infinite(X)).

%% @doc Check if a value is NaN.
-spec is_nan(number()) -> boolean().
is_nan(X) when is_float(X) ->
    %% NaN is the only value not equal to itself
    X /= X;
is_nan(_) ->
    false.

%% @doc Check if a value is infinite.
-spec is_infinite(number()) -> boolean().
is_infinite(X) when is_float(X) ->
    X == X andalso (X * 0.5 == X) andalso X /= 0.0;
is_infinite(_) ->
    false.

%% @doc Safe addition with overflow checking.
-spec safe_add(number(), number()) -> {ok, float()} | {error, atom()}.
safe_add(A, B) when is_number(A), is_number(B) ->
    Result = A + B,
    case is_finite(Result) of
        true -> {ok, Result};
        false when is_nan(Result) -> {error, nan_result};
        false -> {error, overflow}
    end.

%% @doc Safe subtraction with overflow checking.
-spec safe_sub(number(), number()) -> {ok, float()} | {error, atom()}.
safe_sub(A, B) when is_number(A), is_number(B) ->
    Result = A - B,
    case is_finite(Result) of
        true -> {ok, Result};
        false when is_nan(Result) -> {error, nan_result};
        false -> {error, overflow}
    end.

%% @doc Safe multiplication with overflow checking.
-spec safe_mul(number(), number()) -> {ok, float()} | {error, atom()}.
safe_mul(A, B) when is_number(A), is_number(B) ->
    Result = A * B,
    case is_finite(Result) of
        true -> {ok, Result};
        false when is_nan(Result) -> {error, nan_result};
        false -> {error, overflow}
    end.

%% @doc Safe division with zero and overflow checking.
-spec safe_div(number(), number()) -> {ok, float()} | {error, atom()}.
safe_div(_, 0) ->
    {error, division_by_zero};
safe_div(_, 0.0) ->
    {error, division_by_zero};
safe_div(A, B) when is_number(A), is_number(B) ->
    Result = A / B,
    case is_finite(Result) of
        true -> {ok, Result};
        false when is_nan(Result) -> {error, nan_result};
        false -> {error, overflow}
    end.

%% @doc Safe square root (fails for negative numbers).
-spec safe_sqrt(number()) -> {ok, float()} | {error, atom()}.
safe_sqrt(X) when X < 0 ->
    {error, negative_input};
safe_sqrt(X) when is_number(X) ->
    {ok, math:sqrt(X)}.

%% @doc Safe power operation.
-spec safe_pow(number(), number()) -> {ok, float()} | {error, atom()}.
safe_pow(0, Exp) when Exp < 0 ->
    {error, division_by_zero};
safe_pow(Base, Exp) when Base < 0, not is_integer(Exp) ->
    {error, complex_result};
safe_pow(Base, Exp) when is_number(Base), is_number(Exp) ->
    Result = math:pow(Base, Exp),
    case is_finite(Result) of
        true -> {ok, Result};
        false -> {error, overflow}
    end.

%% @doc Safe natural logarithm.
-spec safe_log(number()) -> {ok, float()} | {error, atom()}.
safe_log(X) when X =< 0 ->
    {error, non_positive_input};
safe_log(X) when is_number(X) ->
    {ok, math:log(X)}.

%% @doc Safe base-10 logarithm.
-spec safe_log10(number()) -> {ok, float()} | {error, atom()}.
safe_log10(X) when X =< 0 ->
    {error, non_positive_input};
safe_log10(X) when is_number(X) ->
    {ok, math:log10(X)}.

%% @doc Check if two floats are approximately equal within epsilon.
-spec approximately_equal(float(), float(), float()) -> boolean().
approximately_equal(A, B, Epsilon) when is_float(A), is_float(B), Epsilon >= 0 ->
    Diff = abs(A - B),
    case A == B of
        true -> true;
        false ->
            %% Relative error for large values, absolute for small
            MaxAbs = max(abs(A), abs(B)),
            case MaxAbs < 1.0 of
                true -> Diff =< Epsilon;
                false -> Diff =< Epsilon * MaxAbs
            end
    end.

%% @doc Round a float to N decimal places.
-spec round_to(float(), non_neg_integer()) -> float().
round_to(X, Decimals) when is_float(X), is_integer(Decimals), Decimals >= 0 ->
    Multiplier = math:pow(10, Decimals),
    round(X * Multiplier) / Multiplier.

%% @doc Truncate a float to N decimal places.
-spec truncate_to(float(), non_neg_integer()) -> float().
truncate_to(X, Decimals) when is_float(X), is_integer(Decimals), Decimals >= 0 ->
    Multiplier = math:pow(10, Decimals),
    trunc(X * Multiplier) / Multiplier.

%% @doc Floor a float to N decimal places.
-spec floor_to(float(), non_neg_integer()) -> float().
floor_to(X, Decimals) when is_float(X), is_integer(Decimals), Decimals >= 0 ->
    Multiplier = math:pow(10, Decimals),
    floor(X * Multiplier) / Multiplier.

%% @doc Ceiling a float to N decimal places.
-spec ceiling_to(float(), non_neg_integer()) -> float().
ceiling_to(X, Decimals) when is_float(X), is_integer(Decimals), Decimals >= 0 ->
    Multiplier = math:pow(10, Decimals),
    ceil(X * Multiplier) / Multiplier.

%% @doc Clamp a value to a range.
-spec clamp(number(), number(), number()) -> number().
clamp(X, Min, _Max) when X < Min -> Min;
clamp(X, _Min, Max) when X > Max -> Max;
clamp(X, _Min, _Max) -> X.

%% @doc Linear interpolation between two values.
%% lerp(A, B, 0.0) = A, lerp(A, B, 1.0) = B
-spec lerp(float(), float(), float()) -> float().
lerp(A, B, T) when is_float(T), T >= 0.0, T =< 1.0 ->
    A + (B - A) * T;
lerp(A, _B, T) when T < 0.0 ->
    A;
lerp(_A, B, T) when T > 1.0 ->
    B;
lerp(A, B, T) ->
    A + (B - A) * T.

%% @doc Normalize a value from one range to another.
%% Maps value from [in_min, in_max] to [out_min, out_max].
-spec normalize(float(), {float(), float()}, {float(), float()}) -> float().
normalize(Value, {InMin, InMax}, {OutMin, OutMax}) when InMax /= InMin ->
    Normalized = (Value - InMin) / (InMax - InMin),
    OutMin + Normalized * (OutMax - OutMin).
