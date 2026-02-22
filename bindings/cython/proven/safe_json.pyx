# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

# cython: language_level=3

"""
SafeJson - JSON validation and type detection.

All functions delegate to libproven via direct C FFI calls.
Returns None on error. NEVER reimplements logic.
"""

from libc.stdint cimport uint8_t, int32_t
from libc.stddef cimport size_t

cimport proven.lib_proven as c


# JSON type constants (from ProvenJsonType enum)
JSON_NULL    =  0
JSON_BOOL    =  1
JSON_NUMBER  =  2
JSON_STRING  =  3
JSON_ARRAY   =  4
JSON_OBJECT  =  5
JSON_INVALID = -1


def is_valid_json(str s):
    """Check if a string is valid JSON.
    Returns True if valid, False if invalid, None on FFI error.

    >>> is_valid_json('{"key": 1}')
    True
    >>> is_valid_json("not json")
    False
    """
    cdef bytes b = s.encode("utf-8")
    cdef const uint8_t* ptr = <const uint8_t*><char*>b
    cdef size_t length = len(b)
    cdef c.ProvenBoolResult result
    with nogil:
        result = c.proven_json_is_valid(ptr, length)
    if result.status == c.PROVEN_OK:
        return bool(result.value)
    return None


def json_type(str s):
    """Get the JSON value type at root level.
    Returns one of: JSON_NULL, JSON_BOOL, JSON_NUMBER, JSON_STRING,
    JSON_ARRAY, JSON_OBJECT, or JSON_INVALID.

    >>> json_type("42")
    2
    >>> json_type("not json")
    -1
    """
    cdef bytes b = s.encode("utf-8")
    cdef const uint8_t* ptr = <const uint8_t*><char*>b
    cdef size_t length = len(b)
    cdef int32_t result
    with nogil:
        result = <int32_t>c.proven_json_get_type(ptr, length)
    return result


def json_type_name(int type_code):
    """Convert a JSON type code to a human-readable name.

    >>> json_type_name(JSON_OBJECT)
    'object'
    """
    names = {
        JSON_NULL: "null",
        JSON_BOOL: "boolean",
        JSON_NUMBER: "number",
        JSON_STRING: "string",
        JSON_ARRAY: "array",
        JSON_OBJECT: "object",
        JSON_INVALID: "invalid",
    }
    return names.get(type_code, "unknown")
