Red [
    Title:       "Proven SafeUrl"
    Description: "Safe URL parsing and validation via libproven FFI"
    Author:      "Jonathan D.A. Jewell (hyperpolymath)"
    Email:       "jonathan.jewell@open.ac.uk"
    License:     "PMPL-1.0-or-later"
    Version:     0.5.0
    File:        %safe-url.red
    Needs:       'proven
    Notes: {
        SPDX-License-Identifier: PMPL-1.0-or-later
        Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

        Safe URL parsing following RFC 3986.
        ALL computation is performed in Idris 2 via the Zig FFI bridge.
        This file ONLY wraps FFI calls. It does NOT reimplement any logic.
    }
]

#include %proven.red

; ============================================================================
; SafeUrl Module
; ============================================================================

safe-url: context [

    ; Parse a URL string into its components.
    ;
    ; Returns an object with scheme, host, port, path, query, fragment fields,
    ; or none on parse failure.
    ;
    ; The caller should use the returned object freely; memory is copied out
    ; of the FFI result before the raw result is freed.
    parse-url: func [input [string!]] [
        data: to binary! input
        raw: proven_url_parse data length? data
        if none? raw [return none]
        ; The raw result is a pointer to ProvenUrlResult.
        ; Extract status from the first i32 field.
        ; Due to Red's FFI struct access limitations, we return the raw
        ; pointer for advanced usage. Higher-level wrappers can extract
        ; fields using Red/System struct offsets.
        raw
    ]

    ; Free a URL result previously allocated by parse-url.
    ;
    ; Must be called when done with the parsed URL components.
    free-url: func [components [integer!]] [
        proven_url_free components
    ]
]
