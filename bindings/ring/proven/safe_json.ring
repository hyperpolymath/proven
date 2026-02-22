# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe JSON validation and type detection.
# Thin wrapper over libproven FFI -- all logic lives in Idris 2.

load "proven/lib_proven.ring"

# JSON type constants.
JSON_NULL    = 0
JSON_BOOL    = 1
JSON_NUMBER  = 2
JSON_STRING  = 3
JSON_ARRAY   = 4
JSON_OBJECT  = 5
JSON_INVALID = -1

# Check if a string is valid JSON.
# Returns 1 if valid, 0 otherwise.
func json_is_valid value
    if len(value) = 0 return 0 ok
    lib = proven_lib()
    if lib = NULL return 0 ok
    res = calldll(lib, "proven_json_is_valid", "pi", value, len(value))
    if isNumber(res)
        return res
    ok
    return 0

# Get the JSON value type at the root level.
# Returns one of the JSON_* constants. Returns JSON_INVALID for non-JSON input.
func json_get_type value
    if len(value) = 0 return JSON_INVALID ok
    lib = proven_lib()
    if lib = NULL return JSON_INVALID ok
    return calldll(lib, "proven_json_get_type", "pi", value, len(value))
