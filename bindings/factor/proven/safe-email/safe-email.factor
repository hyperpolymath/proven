! SPDX-License-Identifier: PMPL-1.0-or-later
! Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
!
! proven.safe-email - Email validation via libproven FFI.
! All computation is performed in verified Idris 2 code via the Zig FFI
! layer. This vocabulary is a thin wrapper; it does NOT reimplement any logic.

USING: kernel sequences proven.ffi accessors ;
IN: proven.safe-email

! Validate an email address (RFC 5321 simplified) via libproven.
! Returns the validation result and a success flag.
! ( string -- bool t ) or ( string -- f f )
: valid-email? ( string -- bool/f ? )
    dup length proven_email_is_valid
    dup status>> PROVEN-OK = [
        value>> 0 = not t
    ] [
        drop f f
    ] if ;
