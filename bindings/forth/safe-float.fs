\ SPDX-License-Identifier: PMPL-1.0-or-later
\ Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
\
\ Proven SafeFloat - FFI bindings to libproven float operations.
\ All float operations are performed in verified Idris 2 code via libproven.

c-library proven_float
s" proven" add-lib

\ ProvenFloatResult is { int32_t status; double value; }
\ We use separate calls returning struct members.

c-function proven-float-div    proven_float_div    r r -- a
c-function proven-float-sqrt   proven_float_sqrt   r -- a
c-function proven-float-ln     proven_float_ln     r -- a
c-function proven-float-finite proven_float_is_finite r -- n
c-function proven-float-nan    proven_float_is_nan    r -- n

end-c-library

\ Safe float division via libproven
\ ( F: a b -- ) ( -- status ) ( F: -- result )
: safe-fdiv ( F: a b -- F: result ; -- status )
    proven-float-div ;

\ Safe square root via libproven
\ ( F: x -- ) ( -- status ) ( F: -- result )
: safe-fsqrt ( F: x -- F: result ; -- status )
    proven-float-sqrt ;

\ Safe natural logarithm via libproven
\ ( F: x -- ) ( -- status ) ( F: -- result )
: safe-fln ( F: x -- F: result ; -- status )
    proven-float-ln ;

\ Check if float is finite via libproven
\ ( F: x -- ) ( -- flag )
: float-finite? ( F: x -- ; -- flag )
    proven-float-finite 0<> ;

\ Check if float is NaN via libproven
\ ( F: x -- ) ( -- flag )
: float-nan? ( F: x -- ; -- flag )
    proven-float-nan 0<> ;
