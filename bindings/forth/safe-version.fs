\ SPDX-License-Identifier: PMPL-1.0-or-later
\ Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
\
\ Proven SafeVersion - FFI bindings to libproven version operations.
\ All semantic version operations are performed in verified Idris 2 code
\ via libproven.

c-library proven_version
s" proven" add-lib

\ ProvenVersionResult is { int32_t status; major; minor; patch; prerelease }
c-function proven-ver-parse   proven_version_parse   a n -- a
c-function proven-ver-compare proven_version_compare a a -- n

end-c-library

\ Parse semantic version string via libproven
\ ( c-addr len -- status major minor patch prerelease-addr prerelease-len )
: parse-version ( c-addr len -- status major minor patch pre-addr pre-len )
    proven-ver-parse ;

\ Compare two semantic versions via libproven
\ ( ver-a ver-b -- order )
\ order: negative if a < b, 0 if equal, positive if a > b
: version-compare ( ver-a ver-b -- order )
    proven-ver-compare ;
