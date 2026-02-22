Red [
    Title:       "Proven SafeString"
    Description: "Safe string operations via libproven FFI"
    Author:      "Jonathan D.A. Jewell (hyperpolymath)"
    Email:       "jonathan.jewell@open.ac.uk"
    License:     "PMPL-1.0-or-later"
    Version:     0.5.0
    File:        %safe-string.red
    Needs:       'proven
    Notes: {
        SPDX-License-Identifier: PMPL-1.0-or-later
        Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

        Safe string operations that prevent injection attacks.
        Provides UTF-8 validation and injection-safe escaping for SQL, HTML,
        and JavaScript contexts.
        ALL computation is performed in Idris 2 via the Zig FFI bridge.
        This file ONLY wraps FFI calls. It does NOT reimplement any logic.
    }
]

#include %proven.red

; ============================================================================
; SafeString Module
; ============================================================================

safe-string: context [

    ; Check if a byte sequence is valid UTF-8.
    ;
    ; Returns true if valid UTF-8, false if invalid, or none on FFI failure.
    is-valid-utf8: func [data [binary!]] [
        result: proven_string_is_valid_utf8 data length? data
        either succeeded? result/status [result/value <> 0][none]
    ]

    ; Escape a string for safe use in SQL queries.
    ;
    ; Escapes single quotes by doubling them.
    ; Returns the escaped string, or none on failure.
    ; The caller should treat this as read-only; libproven manages the memory.
    escape-sql: func [input [string!]] [
        data: to binary! input
        result: proven_string_escape_sql data length? data
        either succeeded? result/status [
            str: copy/part result/ptr result/len
            proven_free_string result/ptr
            str
        ][none]
    ]

    ; Escape a string for safe use in HTML content.
    ;
    ; Escapes <, >, &, ", and ' characters.
    ; Returns the escaped string, or none on failure.
    escape-html: func [input [string!]] [
        data: to binary! input
        result: proven_string_escape_html data length? data
        either succeeded? result/status [
            str: copy/part result/ptr result/len
            proven_free_string result/ptr
            str
        ][none]
    ]

    ; Escape a string for safe use in JavaScript string literals.
    ;
    ; Returns the escaped string, or none on failure.
    escape-js: func [input [string!]] [
        data: to binary! input
        result: proven_string_escape_js data length? data
        either succeeded? result/status [
            str: copy/part result/ptr result/len
            proven_free_string result/ptr
            str
        ][none]
    ]
]
