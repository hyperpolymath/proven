Red [
    Title:       "Proven SafePath"
    Description: "Safe filesystem path operations via libproven FFI"
    Author:      "Jonathan D.A. Jewell (hyperpolymath)"
    Email:       "jonathan.jewell@open.ac.uk"
    License:     "PMPL-1.0-or-later"
    Version:     0.5.0
    File:        %safe-path.red
    Needs:       'proven
    Notes: {
        SPDX-License-Identifier: PMPL-1.0-or-later
        Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

        Safe filesystem path operations that prevent traversal attacks.
        ALL computation is performed in Idris 2 via the Zig FFI bridge.
        This file ONLY wraps FFI calls. It does NOT reimplement any logic.
    }
]

#include %proven.red

; ============================================================================
; SafePath Module
; ============================================================================

safe-path: context [

    ; Check if a path contains directory traversal sequences (e.g., "../").
    ;
    ; Returns true if traversal is detected, false if the path is safe,
    ; or none on FFI failure.
    has-traversal?: func [path [string!]] [
        data: to binary! path
        result: proven_path_has_traversal data length? data
        either succeeded? result/status [result/value <> 0][none]
    ]

    ; Sanitize a filename by removing dangerous characters.
    ;
    ; Returns the sanitized filename as a string, or none on failure.
    sanitize-filename: func [filename [string!]] [
        data: to binary! filename
        result: proven_path_sanitize_filename data length? data
        either succeeded? result/status [
            str: copy/part result/ptr result/len
            proven_free_string result/ptr
            str
        ][none]
    ]
]
