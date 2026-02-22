\ SPDX-License-Identifier: PMPL-1.0-or-later
\ Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
\
\ Proven SafeAngle - FFI bindings to libproven angle operations.
\ All angle operations are performed in verified Idris 2 code via libproven.

c-library proven_angle
s" proven" add-lib

c-function proven-deg-to-rad       proven_angle_deg_to_rad       r -- r
c-function proven-rad-to-deg       proven_angle_rad_to_deg       r -- r
c-function proven-normalize-deg    proven_angle_normalize_degrees r -- r
c-function proven-normalize-rad    proven_angle_normalize_radians r -- r

end-c-library

\ Convert degrees to radians via libproven
\ ( F: degrees -- radians )
: deg>rad ( F: degrees -- radians )
    proven-deg-to-rad ;

\ Convert radians to degrees via libproven
\ ( F: radians -- degrees )
: rad>deg ( F: radians -- degrees )
    proven-rad-to-deg ;

\ Normalize angle to [0, 360) degrees via libproven
\ ( F: degrees -- normalized )
: normalize-degrees ( F: degrees -- normalized )
    proven-normalize-deg ;

\ Normalize angle to [0, 2*pi) radians via libproven
\ ( F: radians -- normalized )
: normalize-radians ( F: radians -- normalized )
    proven-normalize-rad ;
