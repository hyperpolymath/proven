\ SPDX-License-Identifier: PMPL-1.0-or-later
\ Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
\
\ Proven SafeUnit - FFI bindings to libproven unit conversion operations.
\ All unit conversions are performed in verified Idris 2 code via libproven.

c-library proven_unit
s" proven" add-lib

\ ProvenFloatResult is { int32_t status; double value; }
c-function proven-length-conv proven_unit_convert_length r n n -- a
c-function proven-temp-conv   proven_unit_convert_temp   r n n -- a

end-c-library

\ Length unit constants (matching ProvenLengthUnit enum)
0 constant LENGTH-METERS
1 constant LENGTH-KILOMETERS
2 constant LENGTH-CENTIMETERS
3 constant LENGTH-MILLIMETERS
4 constant LENGTH-FEET
5 constant LENGTH-INCHES
6 constant LENGTH-MILES
7 constant LENGTH-YARDS

\ Temperature unit constants (matching ProvenTempUnit enum)
0 constant TEMP-CELSIUS
1 constant TEMP-FAHRENHEIT
2 constant TEMP-KELVIN

\ Convert length between units via libproven
\ ( F: value -- F: result ) ( from-unit to-unit -- status )
: convert-length ( F: value -- F: result ; from to -- status )
    proven-length-conv ;

\ Convert temperature between units via libproven
\ ( F: value -- F: result ) ( from-unit to-unit -- status )
: convert-temp ( F: value -- F: result ; from to -- status )
    proven-temp-conv ;
