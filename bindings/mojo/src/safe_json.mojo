# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeJson -- JSON validation and type detection.

Validates JSON strings and detects the root-level JSON value type.
All computation is delegated to the formally verified Idris 2 core via C FFI.
No logic is reimplemented here.

Functions:
    is_valid_json -- Check if a string is valid JSON
    json_type     -- Get the root-level JSON value type
"""

from memory import UnsafePointer

from .lib_proven import (
    BoolResult,
    proven_json_is_valid,
    proven_json_get_type,
    PROVEN_OK,
    PROVEN_JSON_NULL,
    PROVEN_JSON_BOOL,
    PROVEN_JSON_NUMBER,
    PROVEN_JSON_STRING,
    PROVEN_JSON_ARRAY,
    PROVEN_JSON_OBJECT,
    PROVEN_JSON_INVALID,
)


# =============================================================================
# JSON type enumeration (Mojo-idiomatic aliases)
# =============================================================================

alias JSON_NULL: Int32 = PROVEN_JSON_NULL
alias JSON_BOOL: Int32 = PROVEN_JSON_BOOL
alias JSON_NUMBER: Int32 = PROVEN_JSON_NUMBER
alias JSON_STRING: Int32 = PROVEN_JSON_STRING
alias JSON_ARRAY: Int32 = PROVEN_JSON_ARRAY
alias JSON_OBJECT: Int32 = PROVEN_JSON_OBJECT
alias JSON_INVALID: Int32 = PROVEN_JSON_INVALID


fn _str_to_ptr(s: String) -> (UnsafePointer[UInt8], Int):
    """Extract a raw byte pointer and length from a Mojo String."""
    var byte_slice = s.as_bytes()
    var length = len(byte_slice)
    var ptr = byte_slice.unsafe_ptr()
    return (ptr, length)


fn is_valid_json(data: String) -> Bool:
    """Check if a string is valid JSON.

    Returns True if the string parses as valid JSON, False otherwise.
    On error, returns False as a safe default.
    All computation performed in formally verified Idris 2 code.
    """
    var pair = _str_to_ptr(data)
    var result = proven_json_is_valid(pair[0], pair[1])
    if result.succeeded():
        return result.value
    return False


fn json_type(data: String) -> Int32:
    """Get the root-level JSON value type.

    Returns one of the JSON_* constants:
        JSON_NULL    (0)  -- null
        JSON_BOOL    (1)  -- true or false
        JSON_NUMBER  (2)  -- numeric value
        JSON_STRING  (3)  -- string value
        JSON_ARRAY   (4)  -- array
        JSON_OBJECT  (5)  -- object
        JSON_INVALID (-1) -- not valid JSON

    All computation performed in formally verified Idris 2 code.
    """
    var pair = _str_to_ptr(data)
    return proven_json_get_type(pair[0], pair[1])


fn json_type_name(type_code: Int32) -> String:
    """Convert a JSON type code to a human-readable string.

    This is a trivial mapping helper and does not call libproven.
    """
    if type_code == JSON_NULL:
        return "null"
    if type_code == JSON_BOOL:
        return "bool"
    if type_code == JSON_NUMBER:
        return "number"
    if type_code == JSON_STRING:
        return "string"
    if type_code == JSON_ARRAY:
        return "array"
    if type_code == JSON_OBJECT:
        return "object"
    return "invalid"
