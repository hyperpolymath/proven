\ SPDX-License-Identifier: PMPL-1.0-or-later
\ Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
\
\ Proven SafeUUID - FFI bindings to libproven UUID operations.
\ All UUID operations are performed in verified Idris 2 code via libproven.

c-library proven_uuid
s" proven" add-lib

\ ProvenUUIDResult is { int32_t status; uint8_t bytes[16]; }
c-function proven-uuid-v4        proven_uuid_v4          -- a
c-function proven-uuid-to-string proven_uuid_to_string   a -- a
c-function proven-uuid-parse     proven_uuid_parse       a n -- a
c-function proven-uuid-nil       proven_uuid_is_nil      a -- n
c-function proven-uuid-version   proven_uuid_version     a -- n
c-function proven-free-string    proven_free_string      a -- void

end-c-library

\ Generate UUID v4 (random) via libproven
\ ( -- uuid-addr status )
: uuid-v4 ( -- uuid-addr status )
    proven-uuid-v4 ;

\ Format UUID as canonical string via libproven
\ ( uuid-addr -- c-addr len status )
: uuid>string ( uuid-addr -- c-addr len status )
    proven-uuid-to-string ;

\ Parse UUID from string via libproven
\ ( c-addr len -- uuid-addr status )
: parse-uuid ( c-addr len -- uuid-addr status )
    proven-uuid-parse ;

\ Check if UUID is nil (all zeros) via libproven
\ ( uuid-addr -- flag )
: uuid-nil? ( uuid-addr -- flag )
    proven-uuid-nil 0<> ;

\ Get UUID version via libproven
\ ( uuid-addr -- version )
: uuid-version ( uuid-addr -- version )
    proven-uuid-version ;
