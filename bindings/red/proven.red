Red [
    Title:       "Proven FFI Bindings"
    Description: "Red language bindings for libproven - formally verified safety library"
    Author:      "Jonathan D.A. Jewell (hyperpolymath)"
    Email:       "jonathan.jewell@open.ac.uk"
    License:     "PMPL-1.0-or-later"
    Version:     0.5.0
    File:        %proven.red
    Notes: {
        SPDX-License-Identifier: PMPL-1.0-or-later
        Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

        Main module for proven FFI bindings.
        ALL computation is performed in Idris 2 via the Zig FFI bridge.
        This file ONLY declares the C ABI imports. It does NOT reimplement logic.

        Link against libproven.so (Linux) or libproven.dylib (macOS).
    }
]

; ============================================================================
; Status Codes
; ============================================================================

; Status codes returned by libproven operations.
; Zero indicates success; negative values indicate specific error conditions.
STATUS_OK:                    0
STATUS_ERR_NULL_POINTER:     -1
STATUS_ERR_INVALID_ARGUMENT: -2
STATUS_ERR_OVERFLOW:         -3
STATUS_ERR_UNDERFLOW:        -4
STATUS_ERR_DIVISION_BY_ZERO: -5
STATUS_ERR_PARSE_FAILURE:    -6
STATUS_ERR_VALIDATION_FAILED: -7
STATUS_ERR_OUT_OF_BOUNDS:    -8
STATUS_ERR_ENCODING_ERROR:   -9
STATUS_ERR_ALLOCATION_FAILED: -10
STATUS_ERR_NOT_IMPLEMENTED:  -99

; ============================================================================
; JSON Type Constants
; ============================================================================

JSON_NULL:    0
JSON_BOOL:    1
JSON_NUMBER:  2
JSON_STRING:  3
JSON_ARRAY:   4
JSON_OBJECT:  5
JSON_INVALID: -1

; ============================================================================
; C Library Import
; ============================================================================

; Detect platform-specific library name.
lib-name: either system/platform = 'macOS ["libproven.dylib"]["libproven.so"]

; Import all libproven C ABI functions.
; Each function delegates to Idris 2 verified code via the Zig FFI bridge.
#import [lib-name cdecl [

    ; --- Lifecycle ---
    proven_init: "proven_init" [
        return: [integer!]
    ]
    proven_deinit: "proven_deinit" []
    proven_is_initialized: "proven_is_initialized" [
        return: [logic!]
    ]

    ; --- Memory ---
    proven_free_string: "proven_free_string" [
        ptr     [integer!]  ; char* (pointer as integer)
    ]

    ; --- Version ---
    proven_ffi_abi_version: "proven_ffi_abi_version" [
        return: [integer!]
    ]
    proven_version_major: "proven_version_major" [
        return: [integer!]
    ]
    proven_version_minor: "proven_version_minor" [
        return: [integer!]
    ]
    proven_version_patch: "proven_version_patch" [
        return: [integer!]
    ]
    proven_module_count: "proven_module_count" [
        return: [integer!]
    ]

    ; --- SafeMath ---
    ; Note: IntResult is {status: i32, value: i64}. Red receives struct via
    ; pointer-based calling convention. We use individual calls that return
    ; status and value separately, or use struct access.
    proven_math_add_checked: "proven_math_add_checked" [
        a       [integer!]
        b       [integer!]
        return: [integer!]  ; Pointer to IntResult struct
    ]
    proven_math_sub_checked: "proven_math_sub_checked" [
        a       [integer!]
        b       [integer!]
        return: [integer!]
    ]
    proven_math_mul_checked: "proven_math_mul_checked" [
        a       [integer!]
        b       [integer!]
        return: [integer!]
    ]
    proven_math_div: "proven_math_div" [
        num     [integer!]
        den     [integer!]
        return: [integer!]
    ]
    proven_math_mod: "proven_math_mod" [
        num     [integer!]
        den     [integer!]
        return: [integer!]
    ]
    proven_math_abs_safe: "proven_math_abs_safe" [
        n       [integer!]
        return: [integer!]
    ]
    proven_math_clamp: "proven_math_clamp" [
        lo      [integer!]
        hi      [integer!]
        value   [integer!]
        return: [integer!]
    ]
    proven_math_pow_checked: "proven_math_pow_checked" [
        base    [integer!]
        exp     [integer!]
        return: [integer!]
    ]

    ; --- SafeString ---
    proven_string_is_valid_utf8: "proven_string_is_valid_utf8" [
        ptr     [integer!]
        len     [integer!]
        return: [integer!]
    ]
    proven_string_escape_sql: "proven_string_escape_sql" [
        ptr     [integer!]
        len     [integer!]
        return: [integer!]
    ]
    proven_string_escape_html: "proven_string_escape_html" [
        ptr     [integer!]
        len     [integer!]
        return: [integer!]
    ]
    proven_string_escape_js: "proven_string_escape_js" [
        ptr     [integer!]
        len     [integer!]
        return: [integer!]
    ]

    ; --- SafePath ---
    proven_path_has_traversal: "proven_path_has_traversal" [
        ptr     [integer!]
        len     [integer!]
        return: [integer!]
    ]
    proven_path_sanitize_filename: "proven_path_sanitize_filename" [
        ptr     [integer!]
        len     [integer!]
        return: [integer!]
    ]

    ; --- SafeEmail ---
    proven_email_is_valid: "proven_email_is_valid" [
        ptr     [integer!]
        len     [integer!]
        return: [integer!]
    ]

    ; --- SafeUrl ---
    proven_url_parse: "proven_url_parse" [
        ptr     [integer!]
        len     [integer!]
        return: [integer!]
    ]
    proven_url_free: "proven_url_free" [
        comp    [integer!]
    ]

    ; --- SafeCrypto ---
    proven_crypto_constant_time_eq: "proven_crypto_constant_time_eq" [
        ptr1    [integer!]
        len1    [integer!]
        ptr2    [integer!]
        len2    [integer!]
        return: [integer!]
    ]
    proven_crypto_random_bytes: "proven_crypto_random_bytes" [
        ptr     [integer!]
        len     [integer!]
        return: [integer!]
    ]

    ; --- SafeJson ---
    proven_json_is_valid: "proven_json_is_valid" [
        ptr     [integer!]
        len     [integer!]
        return: [integer!]
    ]
    proven_json_get_type: "proven_json_get_type" [
        ptr     [integer!]
        len     [integer!]
        return: [integer!]
    ]

    ; --- SafeFloat ---
    proven_float_div: "proven_float_div" [
        a       [float!]
        b       [float!]
        return: [integer!]
    ]
    proven_float_is_finite: "proven_float_is_finite" [
        x       [float!]
        return: [logic!]
    ]
    proven_float_is_nan: "proven_float_is_nan" [
        x       [float!]
        return: [logic!]
    ]
    proven_float_sqrt: "proven_float_sqrt" [
        x       [float!]
        return: [integer!]
    ]
    proven_float_ln: "proven_float_ln" [
        x       [float!]
        return: [integer!]
    ]

    ; --- SafeHex ---
    proven_hex_encode: "proven_hex_encode" [
        ptr     [integer!]
        len     [integer!]
        upper   [logic!]
        return: [integer!]
    ]

    ; --- SafeChecksum ---
    proven_checksum_crc32: "proven_checksum_crc32" [
        ptr     [integer!]
        len     [integer!]
        return: [integer!]
    ]
    proven_checksum_verify_crc32: "proven_checksum_verify_crc32" [
        ptr      [integer!]
        len      [integer!]
        expected [integer!]
        return:  [integer!]
    ]

    ; --- SafeDateTime ---
    proven_datetime_is_leap_year: "proven_datetime_is_leap_year" [
        year    [integer!]
        return: [logic!]
    ]
    proven_datetime_days_in_month: "proven_datetime_days_in_month" [
        year    [integer!]
        month   [integer!]
        return: [integer!]
    ]
]]

; ============================================================================
; Helper Functions
; ============================================================================

; Check if a status code indicates success.
succeeded?: func [status [integer!]] [
    status = STATUS_OK
]

; Check if a status code indicates failure.
failed?: func [status [integer!]] [
    status <> STATUS_OK
]

; Convert a status code to a human-readable error string.
status-to-string: func [status [integer!]] [
    switch status [
        0   ["OK"]
        -1  ["null pointer"]
        -2  ["invalid argument"]
        -3  ["overflow"]
        -4  ["underflow"]
        -5  ["division by zero"]
        -6  ["parse failure"]
        -7  ["validation failed"]
        -8  ["out of bounds"]
        -9  ["encoding error"]
        -10 ["allocation failed"]
        -99 ["not implemented"]
    ]
]
