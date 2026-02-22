%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeMath - FFI bindings to libproven math operations.
%% All arithmetic is performed in verified Idris 2 code via libproven.

:- module(proven_math, [
    safe_add/3,
    safe_sub/3,
    safe_mul/3,
    safe_div/3,
    safe_mod/3,
    safe_abs/2,
    safe_pow/3,
    clamp/4
]).

:- use_foreign_library(foreign(libproven)).

%% FFI declarations for libproven math functions.
%% Each returns a status code and a value via output arguments.
:- foreign(proven_math_add_checked_ffi, c,
           proven_math_add_checked(+integer, +integer, [-integer], [-integer])).
:- foreign(proven_math_sub_checked_ffi, c,
           proven_math_sub_checked(+integer, +integer, [-integer], [-integer])).
:- foreign(proven_math_mul_checked_ffi, c,
           proven_math_mul_checked(+integer, +integer, [-integer], [-integer])).
:- foreign(proven_math_div_ffi, c,
           proven_math_div(+integer, +integer, [-integer], [-integer])).
:- foreign(proven_math_mod_ffi, c,
           proven_math_mod(+integer, +integer, [-integer], [-integer])).
:- foreign(proven_math_abs_safe_ffi, c,
           proven_math_abs_safe(+integer, [-integer], [-integer])).
:- foreign(proven_math_clamp_ffi, c,
           proven_math_clamp(+integer, +integer, +integer, [-integer])).
:- foreign(proven_math_pow_checked_ffi, c,
           proven_math_pow_checked(+integer, +integer, [-integer], [-integer])).

%! safe_add(+A, +B, -Result) is det.
%
%  Checked addition via libproven.
%  Result is ok(Sum) or error(overflow).
safe_add(A, B, Result) :-
    proven_math_add_checked_ffi(A, B, Status, Value),
    (   Status =:= 0
    ->  Result = ok(Value)
    ;   Result = error(overflow)
    ).

%! safe_sub(+A, +B, -Result) is det.
%
%  Checked subtraction via libproven.
%  Result is ok(Difference) or error(overflow).
safe_sub(A, B, Result) :-
    proven_math_sub_checked_ffi(A, B, Status, Value),
    (   Status =:= 0
    ->  Result = ok(Value)
    ;   Result = error(overflow)
    ).

%! safe_mul(+A, +B, -Result) is det.
%
%  Checked multiplication via libproven.
%  Result is ok(Product) or error(overflow).
safe_mul(A, B, Result) :-
    proven_math_mul_checked_ffi(A, B, Status, Value),
    (   Status =:= 0
    ->  Result = ok(Value)
    ;   Result = error(overflow)
    ).

%! safe_div(+A, +B, -Result) is det.
%
%  Safe division via libproven.
%  Result is ok(Quotient), error(division_by_zero), or error(overflow).
safe_div(A, B, Result) :-
    proven_math_div_ffi(A, B, Status, Value),
    (   Status =:= 0
    ->  Result = ok(Value)
    ;   Status =:= -5
    ->  Result = error(division_by_zero)
    ;   Result = error(overflow)
    ).

%! safe_mod(+A, +B, -Result) is det.
%
%  Safe modulo via libproven.
%  Result is ok(Remainder) or error(division_by_zero).
safe_mod(A, B, Result) :-
    proven_math_mod_ffi(A, B, Status, Value),
    (   Status =:= 0
    ->  Result = ok(Value)
    ;   Result = error(division_by_zero)
    ).

%! safe_abs(+A, -Result) is det.
%
%  Safe absolute value via libproven.
%  Result is ok(AbsValue) or error(overflow).
safe_abs(A, Result) :-
    proven_math_abs_safe_ffi(A, Status, Value),
    (   Status =:= 0
    ->  Result = ok(Value)
    ;   Result = error(overflow)
    ).

%! safe_pow(+Base, +Exponent, -Result) is det.
%
%  Checked exponentiation via libproven.
%  Result is ok(Power) or error(overflow).
safe_pow(Base, Exp, Result) :-
    proven_math_pow_checked_ffi(Base, Exp, Status, Value),
    (   Status =:= 0
    ->  Result = ok(Value)
    ;   Result = error(overflow)
    ).

%! clamp(+Value, +Lo, +Hi, -Result) is det.
%
%  Clamp value to [Lo, Hi] via libproven.
clamp(Value, Lo, Hi, Result) :-
    proven_math_clamp_ffi(Lo, Hi, Value, Result).
