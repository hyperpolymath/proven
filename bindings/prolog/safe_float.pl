%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeFloat - FFI bindings to libproven floating-point operations.
%% All float operations are performed in verified Idris 2 code via libproven.

:- module(safe_float, [
    float_div/3,
    float_sqrt/2,
    float_ln/2,
    float_is_finite/1,
    float_is_nan/1
]).

:- use_foreign_library(foreign(libproven)).

%% FFI declarations for libproven float functions.
:- foreign(proven_float_div_ffi, c,
           proven_float_div(+float, +float, [-integer], [-float])).
:- foreign(proven_float_sqrt_ffi, c,
           proven_float_sqrt(+float, [-integer], [-float])).
:- foreign(proven_float_ln_ffi, c,
           proven_float_ln(+float, [-integer], [-float])).
:- foreign(proven_float_is_finite_ffi, c,
           proven_float_is_finite(+float, [-integer])).
:- foreign(proven_float_is_nan_ffi, c,
           proven_float_is_nan(+float, [-integer])).

%! float_div(+A, +B, -Result) is det.
%
%  Safe floating-point division via libproven.
%  Result is ok(Quotient), error(division_by_zero), or error(invalid).
float_div(A, B, Result) :-
    proven_float_div_ffi(A, B, Status, Value),
    (   Status =:= 0
    ->  Result = ok(Value)
    ;   Status =:= -5
    ->  Result = error(division_by_zero)
    ;   Result = error(invalid)
    ).

%! float_sqrt(+X, -Result) is det.
%
%  Safe square root via libproven.
%  Result is ok(Root) or error(invalid) if X is negative or NaN.
float_sqrt(X, Result) :-
    proven_float_sqrt_ffi(X, Status, Value),
    (   Status =:= 0
    ->  Result = ok(Value)
    ;   Result = error(invalid)
    ).

%! float_ln(+X, -Result) is det.
%
%  Safe natural logarithm via libproven.
%  Result is ok(Ln) or error(invalid) if X <= 0 or NaN.
float_ln(X, Result) :-
    proven_float_ln_ffi(X, Status, Value),
    (   Status =:= 0
    ->  Result = ok(Value)
    ;   Result = error(invalid)
    ).

%! float_is_finite(+X) is semidet.
%
%  Check if float is finite (not NaN or Inf) via libproven.
float_is_finite(X) :-
    proven_float_is_finite_ffi(X, Value),
    Value =:= 1.

%! float_is_nan(+X) is semidet.
%
%  Check if float is NaN via libproven.
float_is_nan(X) :-
    proven_float_is_nan_ffi(X, Value),
    Value =:= 1.
