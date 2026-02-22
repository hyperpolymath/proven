\ SPDX-License-Identifier: PMPL-1.0-or-later
\ Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
\
\ Proven SafeColor - FFI bindings to libproven color operations.
\ All color operations are performed in verified Idris 2 code via libproven.

c-library proven_color
s" proven" add-lib

\ ProvenColorResult is { int32_t status; uint8_t r, g, b; }
c-function proven-color-parse  proven_color_parse_hex  a n -- a
c-function proven-rgb-to-hsl   proven_color_rgb_to_hsl n n n -- a
c-function proven-color-to-hex proven_color_to_hex     n n n -- a

end-c-library

\ Parse hex color string via libproven
\ ( c-addr len -- status r g b )
: parse-hex-color ( c-addr len -- status r g b )
    proven-color-parse ;

\ Convert RGB to HSL via libproven
\ ( r g b -- ) ( F: -- h s l )
: rgb>hsl ( r g b -- F: h s l )
    proven-rgb-to-hsl ;

\ Format RGB as hex string via libproven
\ ( r g b -- c-addr len status )
: color>hex ( r g b -- c-addr len status )
    proven-color-to-hex ;
