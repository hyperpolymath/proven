Red [
    Title:       "Proven SafeCrypto"
    Description: "Safe cryptographic primitives via libproven FFI"
    Author:      "Jonathan D.A. Jewell (hyperpolymath)"
    Email:       "jonathan.jewell@open.ac.uk"
    License:     "PMPL-1.0-or-later"
    Version:     0.5.0
    File:        %safe-crypto.red
    Needs:       'proven
    Notes: {
        SPDX-License-Identifier: PMPL-1.0-or-later
        Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

        Safe cryptographic operations: constant-time comparison and CSPRNG.
        ALL computation is performed in Idris 2 via the Zig FFI bridge.
        This file ONLY wraps FFI calls. It does NOT reimplement any logic.
    }
]

#include %proven.red

; ============================================================================
; SafeCrypto Module
; ============================================================================

safe-crypto: context [

    ; Constant-time byte comparison (timing-attack resistant).
    ;
    ; Returns true if both binary values are identical, false otherwise.
    ; The comparison takes the same amount of time regardless of where
    ; a mismatch occurs. Returns none on FFI failure.
    constant-time-eq?: func [a [binary!] b [binary!]] [
        result: proven_crypto_constant_time_eq a length? a b length? b
        either succeeded? result/status [result/value <> 0][none]
    ]

    ; Fill a binary buffer with cryptographically secure random bytes.
    ;
    ; Returns true on success, false on failure.
    ; The buffer is modified in-place.
    random-bytes: func [buf [binary!] len [integer!]] [
        status: proven_crypto_random_bytes buf len
        succeeded? status
    ]
]
