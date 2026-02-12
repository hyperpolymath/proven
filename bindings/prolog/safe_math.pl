% SPDX-License-Identifier: Apache-2.0
% SPDX-FileCopyrightText: 2025 Hyperpolymath
%
% Proven SafeMath - Overflow-checked arithmetic for Prolog
% Provides safe integer operations with overflow detection.

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

%% Constants for 64-bit integers
max_int64(9223372036854775807).
min_int64(-9223372036854775808).

%! safe_add(+A, +B, -Result) is det.
%
%  Add two integers with overflow checking.
%  Result is ok(Sum) or error(overflow).
%
%  @arg A First integer operand
%  @arg B Second integer operand
%  @arg Result ok(Sum) or error(overflow)
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

%! safe_sub(+A, +B, -Result) is det.
%
%  Subtract two integers with overflow checking.
%  Result is ok(Difference) or error(overflow).
%
%  @arg A First integer operand
%  @arg B Second integer operand
%  @arg Result ok(Difference) or error(overflow)
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

%! safe_mul(+A, +B, -Result) is det.
%
%  Multiply two integers with overflow checking.
%  Result is ok(Product) or error(overflow).
%
%  @arg A First integer operand
%  @arg B Second integer operand
%  @arg Result ok(Product) or error(overflow)
safe_mul(0, _, ok(0)) :- !.
safe_mul(_, 0, ok(0)) :- !.
safe_mul(A, B, Result) :-
    Product is A * B,
    Check is Product // A,
    (   Check =:= B
    ->  Result = ok(Product)
    ;   Result = error(overflow)
    ).

%! safe_div(+A, +B, -Result) is det.
%
%  Divide two integers with overflow and division-by-zero checking.
%  Result is ok(Quotient) or error(Reason).
%
%  @arg A Dividend
%  @arg B Divisor
%  @arg Result ok(Quotient) or error(division_by_zero) or error(overflow)
safe_div(_, 0, error(division_by_zero)) :- !.
safe_div(A, -1, error(overflow)) :-
    min_int64(Min),
    A =:= Min, !.
safe_div(A, B, ok(Result)) :-
    Result is A // B.

%! safe_mod(+A, +B, -Result) is det.
%
%  Compute modulo with division-by-zero checking.
%  Result is ok(Remainder) or error(division_by_zero).
%
%  @arg A Dividend
%  @arg B Divisor
%  @arg Result ok(Remainder) or error(division_by_zero)
safe_mod(_, 0, error(division_by_zero)) :- !.
safe_mod(A, B, ok(Result)) :-
    Result is A mod B.

%! safe_abs(+A, -Result) is det.
%
%  Compute absolute value with overflow checking.
%  Result is ok(AbsValue) or error(overflow).
%
%  @arg A Integer value
%  @arg Result ok(AbsValue) or error(overflow)
safe_abs(A, error(overflow)) :-
    min_int64(Min),
    A =:= Min, !.
safe_abs(A, ok(Result)) :-
    Result is abs(A).

%! safe_negate(+A, -Result) is det.
%
%  Negate an integer with overflow checking.
%  Result is ok(Negated) or error(overflow).
%
%  @arg A Integer value
%  @arg Result ok(Negated) or error(overflow)
safe_negate(A, error(overflow)) :-
    min_int64(Min),
    A =:= Min, !.
safe_negate(A, ok(Result)) :-
    Result is -A.

%! safe_pow(+Base, +Exponent, -Result) is det.
%
%  Compute power with overflow checking.
%  Result is ok(Power) or error(overflow) or error(negative_exponent).
%
%  @arg Base Base value
%  @arg Exponent Non-negative exponent
%  @arg Result ok(Power) or error(Reason)
safe_pow(_, Exp, error(negative_exponent)) :-
    Exp < 0, !.
safe_pow(_, 0, ok(1)) :- !.
safe_pow(0, _, ok(0)) :- !.
safe_pow(1, _, ok(1)) :- !.
safe_pow(Base, Exp, Result) :-
    safe_pow_acc(Base, Exp, 1, Result).

safe_pow_acc(_, 0, Acc, ok(Acc)) :- !.
safe_pow_acc(Base, Exp, Acc, Result) :-
    safe_mul(Acc, Base, MulResult),
    (   MulResult = ok(NewAcc)
    ->  Exp1 is Exp - 1,
        safe_pow_acc(Base, Exp1, NewAcc, Result)
    ;   Result = error(overflow)
    ).

%! clamp(+Value, +Min, +Max, -Result) is det.
%
%  Clamp Value to be within [Min, Max].
%
%  @arg Value Value to clamp
%  @arg Min Minimum bound
%  @arg Max Maximum bound
%  @arg Result Clamped value
clamp(Value, Min, _, Min) :-
    Value < Min, !.
clamp(Value, _, Max, Max) :-
    Value > Max, !.
clamp(Value, _, _, Value).

%! in_range(+Value, +Min, +Max) is semidet.
%
%  Succeeds if Value is within [Min, Max] inclusive.
%
%  @arg Value Value to check
%  @arg Min Minimum bound
%  @arg Max Maximum bound
in_range(Value, Min, Max) :-
    Value >= Min,
    Value =< Max.

%! checked_increment(+Value, -Result) is det.
%
%  Increment with overflow checking.
%
%  @arg Value Integer to increment
%  @arg Result ok(Incremented) or error(overflow)
checked_increment(Value, Result) :-
    safe_add(Value, 1, Result).

%! checked_decrement(+Value, -Result) is det.
%
%  Decrement with overflow checking.
%
%  @arg Value Integer to decrement
%  @arg Result ok(Decremented) or error(overflow)
checked_decrement(Value, Result) :-
    safe_sub(Value, 1, Result).
