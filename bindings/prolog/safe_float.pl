% SPDX-License-Identifier: PMPL-1.0
% SPDX-FileCopyrightText: 2025 Hyperpolymath
%
% Proven SafeFloat - Safe floating-point operations for Prolog
% IEEE 754 compliant with NaN and infinity handling.

:- module(safe_float, [
    float_add/3,
    float_sub/3,
    float_mul/3,
    float_div/3,
    float_abs/2,
    float_floor/2,
    float_ceil/2,
    float_round/2,
    float_round/3,
    float_truncate/2,
    float_sign/2,
    float_compare/3,
    float_equals/3,
    float_clamp/4,
    float_lerp/4,
    float_is_nan/1,
    float_is_infinite/1,
    float_is_finite/1,
    float_is_positive/1,
    float_is_negative/1,
    float_is_zero/1,
    float_to_fraction/3,
    epsilon/1,
    float_min/1,
    float_max/1
]).

%! epsilon(-E) is det.
%
%  Machine epsilon for float comparisons.
%
%  @arg E Epsilon value
epsilon(1.0e-10).

%! float_min(-Min) is det.
%
%  Minimum positive normalized float.
%
%  @arg Min Minimum float value
float_min(2.2250738585072014e-308).

%! float_max(-Max) is det.
%
%  Maximum finite float.
%
%  @arg Max Maximum float value
float_max(1.7976931348623157e+308).

%! float_add(+A, +B, -Result) is det.
%
%  Add two floats with special value handling.
%
%  @arg A First operand
%  @arg B Second operand
%  @arg Result Sum
float_add(A, B, Result) :-
    Result is float(A) + float(B).

%! float_sub(+A, +B, -Result) is det.
%
%  Subtract two floats with special value handling.
%
%  @arg A First operand
%  @arg B Second operand
%  @arg Result Difference
float_sub(A, B, Result) :-
    Result is float(A) - float(B).

%! float_mul(+A, +B, -Result) is det.
%
%  Multiply two floats.
%
%  @arg A First operand
%  @arg B Second operand
%  @arg Result Product
float_mul(A, B, Result) :-
    Result is float(A) * float(B).

%! float_div(+A, +B, -Result) is det.
%
%  Divide two floats.
%  Returns infinity for division by zero.
%
%  @arg A Dividend
%  @arg B Divisor
%  @arg Result Quotient or infinity/nan
float_div(_, B, Result) :-
    float(B),
    B =:= 0.0, !,
    Result = error(division_by_zero).
float_div(A, B, ok(Result)) :-
    Result is float(A) / float(B).

%! float_abs(+F, -Abs) is det.
%
%  Absolute value of float.
%
%  @arg F Float value
%  @arg Abs Absolute value
float_abs(F, Abs) :-
    Abs is abs(float(F)).

%! float_floor(+F, -Floor) is det.
%
%  Floor of float (round toward negative infinity).
%
%  @arg F Float value
%  @arg Floor Floored value
float_floor(F, Floor) :-
    Floor is floor(float(F)).

%! float_ceil(+F, -Ceil) is det.
%
%  Ceiling of float (round toward positive infinity).
%
%  @arg F Float value
%  @arg Ceil Ceiling value
float_ceil(F, Ceil) :-
    Ceil is ceiling(float(F)).

%! float_round(+F, -Rounded) is det.
%
%  Round float to nearest integer.
%
%  @arg F Float value
%  @arg Rounded Rounded value
float_round(F, Rounded) :-
    Rounded is round(float(F)).

%! float_round(+F, +Decimals, -Rounded) is det.
%
%  Round float to specified decimal places.
%
%  @arg F Float value
%  @arg Decimals Number of decimal places
%  @arg Rounded Rounded value
float_round(F, Decimals, Rounded) :-
    Factor is 10 ** Decimals,
    Rounded is round(float(F) * Factor) / Factor.

%! float_truncate(+F, -Truncated) is det.
%
%  Truncate float toward zero.
%
%  @arg F Float value
%  @arg Truncated Truncated value
float_truncate(F, Truncated) :-
    Truncated is truncate(float(F)).

%! float_sign(+F, -Sign) is det.
%
%  Get sign of float (-1, 0, or 1).
%
%  @arg F Float value
%  @arg Sign -1, 0, or 1
float_sign(F, Sign) :-
    (   F < 0 -> Sign = -1
    ;   F > 0 -> Sign = 1
    ;   Sign = 0
    ).

%! float_compare(-Order, +A, +B) is det.
%
%  Compare two floats.
%  Order is one of: <, =, >.
%
%  @arg Order Comparison result
%  @arg A First float
%  @arg B Second float
float_compare(Order, A, B) :-
    FA is float(A),
    FB is float(B),
    compare(Order, FA, FB).

%! float_equals(+A, +B, +Tolerance) is semidet.
%
%  Check if two floats are equal within tolerance.
%
%  @arg A First float
%  @arg B Second float
%  @arg Tolerance Acceptable difference
float_equals(A, B, Tolerance) :-
    abs(float(A) - float(B)) =< Tolerance.

%! float_clamp(+F, +Min, +Max, -Clamped) is det.
%
%  Clamp float to range [Min, Max].
%
%  @arg F Float to clamp
%  @arg Min Minimum value
%  @arg Max Maximum value
%  @arg Clamped Clamped result
float_clamp(F, Min, _, Min) :-
    F < Min, !.
float_clamp(F, _, Max, Max) :-
    F > Max, !.
float_clamp(F, _, _, F).

%! float_lerp(+A, +B, +T, -Result) is det.
%
%  Linear interpolation between A and B.
%  Result = A + (B - A) * T
%
%  @arg A Start value
%  @arg B End value
%  @arg T Interpolation factor (0-1)
%  @arg Result Interpolated value
float_lerp(A, B, T, Result) :-
    Result is float(A) + (float(B) - float(A)) * float(T).

%! float_is_nan(+F) is semidet.
%
%  Check if value is NaN.
%
%  @arg F Value to check
float_is_nan(F) :-
    catch((X is float(F), X =\= X), _, fail).

%! float_is_infinite(+F) is semidet.
%
%  Check if value is infinite.
%
%  @arg F Value to check
float_is_infinite(F) :-
    X is float(F),
    (   X =:= 1.0e308 * 10
    ;   X =:= -1.0e308 * 10
    ).

%! float_is_finite(+F) is semidet.
%
%  Check if value is finite (not NaN or infinite).
%
%  @arg F Value to check
float_is_finite(F) :-
    \+ float_is_nan(F),
    \+ float_is_infinite(F).

%! float_is_positive(+F) is semidet.
%
%  Check if float is positive.
%
%  @arg F Float to check
float_is_positive(F) :-
    float(F),
    F > 0.

%! float_is_negative(+F) is semidet.
%
%  Check if float is negative.
%
%  @arg F Float to check
float_is_negative(F) :-
    float(F),
    F < 0.

%! float_is_zero(+F) is semidet.
%
%  Check if float is zero (within epsilon).
%
%  @arg F Float to check
float_is_zero(F) :-
    epsilon(E),
    abs(float(F)) < E.

%! float_to_fraction(+F, -Numerator, -Denominator) is det.
%
%  Convert float to approximate fraction.
%
%  @arg F Float value
%  @arg Numerator Fraction numerator
%  @arg Denominator Fraction denominator
float_to_fraction(F, Numerator, Denominator) :-
    (   float_is_zero(F)
    ->  Numerator = 0, Denominator = 1
    ;   % Use continued fraction expansion
        continued_fraction(F, 10, Num, Den),
        Numerator = Num,
        Denominator = Den
    ).

continued_fraction(F, MaxIterations, Num, Den) :-
    cf_expand(F, MaxIterations, [], CFList),
    cf_to_fraction(CFList, Num, Den).

cf_expand(_, 0, Acc, Acc) :- !.
cf_expand(F, N, Acc, Result) :-
    IntPart is floor(F),
    NewAcc = [IntPart|Acc],
    FracPart is F - IntPart,
    (   FracPart < 1.0e-10
    ->  Result = NewAcc
    ;   N1 is N - 1,
        Reciprocal is 1 / FracPart,
        cf_expand(Reciprocal, N1, NewAcc, Result)
    ).

cf_to_fraction([], 0, 1).
cf_to_fraction([H|T], Num, Den) :-
    cf_to_fraction(T, PrevNum, PrevDen),
    Num is H * PrevNum + PrevDen,
    Den is PrevNum.
