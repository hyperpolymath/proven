Red [
    Title:       "Proven SafeJson"
    Description: "Safe JSON validation via libproven FFI"
    Author:      "Jonathan D.A. Jewell (hyperpolymath)"
    Email:       "jonathan.jewell@open.ac.uk"
    License:     "PMPL-1.0-or-later"
    Version:     0.5.0
    File:        %safe-json.red
    Needs:       'proven
    Notes: {
        SPDX-License-Identifier: PMPL-1.0-or-later
        Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

        Safe JSON validation and type detection without full parsing.
        ALL computation is performed in Idris 2 via the Zig FFI bridge.
        This file ONLY wraps FFI calls. It does NOT reimplement any logic.
    }
]

#include %proven.red

; ============================================================================
; SafeJson Module
; ============================================================================

safe-json: context [

    ; Check if a string is valid JSON.
    ;
    ; Returns true if the input is syntactically valid JSON, false otherwise.
    ; Returns none on FFI failure.
    is-valid?: func [input [string!]] [
        data: to binary! input
        result: proven_json_is_valid data length? data
        either succeeded? result/status [result/value <> 0][none]
    ]

    ; Get the JSON value type at the root level.
    ;
    ; Returns one of the JSON_* constants:
    ;   JSON_NULL (0), JSON_BOOL (1), JSON_NUMBER (2), JSON_STRING (3),
    ;   JSON_ARRAY (4), JSON_OBJECT (5), JSON_INVALID (-1).
    get-type: func [input [string!]] [
        data: to binary! input
        proven_json_get_type data length? data
    ]

    ; Convert a JSON type code to a human-readable word.
    ;
    ; Returns a word! value: 'null, 'bool, 'number, 'string, 'array,
    ; 'object, or 'invalid.
    type-name: func [type-code [integer!]] [
        switch type-code [
            0  ['null]
            1  ['bool]
            2  ['number]
            3  ['string]
            4  ['array]
            5  ['object]
            -1 ['invalid]
        ]
    ]
]
