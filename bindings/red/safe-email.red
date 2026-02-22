Red [
    Title:       "Proven SafeEmail"
    Description: "Safe email validation via libproven FFI"
    Author:      "Jonathan D.A. Jewell (hyperpolymath)"
    Email:       "jonathan.jewell@open.ac.uk"
    License:     "PMPL-1.0-or-later"
    Version:     0.5.0
    File:        %safe-email.red
    Needs:       'proven
    Notes: {
        SPDX-License-Identifier: PMPL-1.0-or-later
        Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

        Safe email validation per RFC 5321/5322 simplified rules.
        ALL computation is performed in Idris 2 via the Zig FFI bridge.
        This file ONLY wraps FFI calls. It does NOT reimplement any logic.
    }
]

#include %proven.red

; ============================================================================
; SafeEmail Module
; ============================================================================

safe-email: context [

    ; Validate an email address per RFC 5321 simplified rules.
    ;
    ; Returns true if the email is valid, false if invalid,
    ; or none on FFI failure.
    is-valid?: func [email [string!]] [
        data: to binary! email
        result: proven_email_is_valid data length? data
        either succeeded? result/status [result/value <> 0][none]
    ]
]
