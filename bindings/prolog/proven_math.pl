%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven SafeMath - Overflow-checked arithmetic for Prolog

:- module(proven_math, [
    max_int64/1,
    min_int64/1,
    safe_add/3,
    safe_sub/3,
    safe_mul/3,
    safe_div/3,
    safe_mod/3,
    safe_abs/2,
    safe_negate/2,
    clamp/4,
    in_range/3
]).

%% Constants for 64-bit integers
max_int64(9223372036854775807).
min_int64(-9223372036854775808).

%% safe_add(+A, +B, -Result)
%% Result is ok(Sum) or error(overflow)
safe_add(A, B, error(overflow)) :-
    B > 0,
    max_int64(Max),
    A > Max - B, !.
safe_add(A, B, error(overflow)) :-
    B < 0,
    min_int64(Min),
    A < Min - B, !.
safe_add(A, B, ok(Result)) :-
    Result is A + B.

%% safe_sub(+A, +B, -Result)
%% Result is ok(Difference) or error(overflow)
safe_sub(A, B, error(overflow)) :-
    B < 0,
    max_int64(Max),
    A > Max + B, !.
safe_sub(A, B, error(overflow)) :-
    B > 0,
    min_int64(Min),
    A < Min + B, !.
safe_sub(A, B, ok(Result)) :-
    Result is A - B.

%% safe_mul(+A, +B, -Result)
%% Result is ok(Product) or error(overflow)
safe_mul(0, _, ok(0)) :- !.
safe_mul(_, 0, ok(0)) :- !.
safe_mul(A, B, Result) :-
    Product is A * B,
    Check is Product // A,
    (   Check =:= B
    ->  Result = ok(Product)
    ;   Result = error(overflow)
    ).

%% safe_div(+A, +B, -Result)
%% Result is ok(Quotient) or error(Reason)
safe_div(_, 0, error(division_by_zero)) :- !.
safe_div(A, -1, error(overflow)) :-
    min_int64(Min),
    A =:= Min, !.
safe_div(A, B, ok(Result)) :-
    Result is A // B.

%% safe_mod(+A, +B, -Result)
%% Result is ok(Remainder) or error(division_by_zero)
safe_mod(_, 0, error(division_by_zero)) :- !.
safe_mod(A, B, ok(Result)) :-
    Result is A mod B.

%% safe_abs(+A, -Result)
%% Result is ok(AbsValue) or error(overflow)
safe_abs(A, error(overflow)) :-
    min_int64(Min),
    A =:= Min, !.
safe_abs(A, ok(Result)) :-
    Result is abs(A).

%% safe_negate(+A, -Result)
%% Result is ok(Negated) or error(overflow)
safe_negate(A, error(overflow)) :-
    min_int64(Min),
    A =:= Min, !.
safe_negate(A, ok(Result)) :-
    Result is -A.

%% clamp(+Value, +Min, +Max, -Result)
%% Clamp Value to be within [Min, Max]
clamp(Value, Min, _, Min) :-
    Value < Min, !.
clamp(Value, _, Max, Max) :-
    Value > Max, !.
clamp(Value, _, _, Value).

%% in_range(+Value, +Min, +Max)
%% Succeeds if Value is within [Min, Max]
in_range(Value, Min, Max) :-
    Value >= Min,
    Value =< Max.
