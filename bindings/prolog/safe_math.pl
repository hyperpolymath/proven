%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeMath - Extended FFI bindings to libproven math operations.
%% All arithmetic is performed in verified Idris 2 code via libproven.
%% This module re-exports proven_math predicates and adds convenience helpers.

:- module(safe_math, [
    max_int64/1,
    min_int64/1,
    safe_add/3,
    safe_sub/3,
    safe_mul/3,
    safe_div/3,
    safe_mod/3,
    safe_abs/2,
    safe_negate/2,
    safe_pow/3,
    clamp/4,
    in_range/3,
    checked_increment/2,
    checked_decrement/2
]).

:- use_module(proven_math, [
    safe_add/3 as pm_safe_add,
    safe_sub/3 as pm_safe_sub,
    safe_mul/3 as pm_safe_mul,
    safe_div/3 as pm_safe_div,
    safe_mod/3 as pm_safe_mod,
    safe_abs/2 as pm_safe_abs,
    safe_pow/3 as pm_safe_pow,
    clamp/4 as pm_clamp
]).

%% Constants
max_int64(9223372036854775807).
min_int64(-9223372036854775808).

%% Delegate to proven_math FFI wrappers
safe_add(A, B, R) :- pm_safe_add(A, B, R).
safe_sub(A, B, R) :- pm_safe_sub(A, B, R).
safe_mul(A, B, R) :- pm_safe_mul(A, B, R).
safe_div(A, B, R) :- pm_safe_div(A, B, R).
safe_mod(A, B, R) :- pm_safe_mod(A, B, R).
safe_abs(A, R)    :- pm_safe_abs(A, R).
safe_pow(B, E, R) :- pm_safe_pow(B, E, R).
clamp(V, Lo, Hi, R) :- pm_clamp(V, Lo, Hi, R).

%! safe_negate(+A, -Result) is det.
%
%  Negate via libproven (subtract from 0).
safe_negate(A, Result) :-
    pm_safe_sub(0, A, Result).

%! in_range(+Value, +Min, +Max) is semidet.
%
%  Check if Value is within [Min, Max] using libproven clamp.
in_range(Value, Min, Max) :-
    pm_clamp(Value, Min, Max, Clamped),
    Clamped =:= Value.

%! checked_increment(+Value, -Result) is det.
%
%  Increment with overflow checking via libproven.
checked_increment(Value, Result) :-
    pm_safe_add(Value, 1, Result).

%! checked_decrement(+Value, -Result) is det.
%
%  Decrement with overflow checking via libproven.
checked_decrement(Value, Result) :-
    pm_safe_sub(Value, 1, Result).
