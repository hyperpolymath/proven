# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeString -- Encoding-safe text operations.

Provides safe escaping for SQL, HTML, and JavaScript contexts, plus UTF-8
validation and hex encoding. All computation is delegated to the formally
verified Idris 2 core via C FFI. No logic is reimplemented here.

Functions:
    is_valid_utf8 -- Check if a byte sequence is valid UTF-8
    escape_sql    -- Escape string for SQL (single-quote escaping)
    escape_html   -- Escape string for HTML (XSS prevention)
    escape_js     -- Escape string for JavaScript string literals
    hex_encode    -- Encode bytes to hexadecimal string
"""

from memory import UnsafePointer

from .lib_proven import (
    BoolResult,
    StringResult,
    proven_string_is_valid_utf8,
    proven_string_escape_sql,
    proven_string_escape_html,
    proven_string_escape_js,
    proven_hex_encode,
    proven_free_string,
    string_result_to_string,
    PROVEN_OK,
)


fn _str_to_ptr(s: String) -> (UnsafePointer[UInt8], Int):
    """Extract a raw byte pointer and length from a Mojo String.

    The returned pointer is only valid for the lifetime of the String.
    """
    var byte_slice = s.as_bytes()
    var length = len(byte_slice)
    var ptr = byte_slice.unsafe_ptr()
    return (ptr, length)


fn is_valid_utf8(data: String) -> Bool:
    """Check if data is valid UTF-8.

    All computation performed in formally verified Idris 2 code.
    """
    var pair = _str_to_ptr(data)
    var result = proven_string_is_valid_utf8(pair[0], pair[1])
    if result.succeeded():
        return result.value
    return False


fn escape_sql(input: String) -> Optional[String]:
    """Escape string for SQL single-quote context.

    Returns None on error (e.g. null pointer, encoding error).
    Prefer parameterized queries over string escaping.
    All computation performed in formally verified Idris 2 code.
    """
    var pair = _str_to_ptr(input)
    var result = proven_string_escape_sql(pair[0], pair[1])
    return string_result_to_string(result)


fn escape_html(input: String) -> Optional[String]:
    """Escape string for HTML to prevent XSS attacks.

    Escapes <, >, &, ", and ' characters.
    Returns None on error.
    All computation performed in formally verified Idris 2 code.
    """
    var pair = _str_to_ptr(input)
    var result = proven_string_escape_html(pair[0], pair[1])
    return string_result_to_string(result)


fn escape_js(input: String) -> Optional[String]:
    """Escape string for JavaScript string literals.

    Returns None on error.
    All computation performed in formally verified Idris 2 code.
    """
    var pair = _str_to_ptr(input)
    var result = proven_string_escape_js(pair[0], pair[1])
    return string_result_to_string(result)


fn hex_encode(data: String, uppercase: Bool = False) -> Optional[String]:
    """Encode bytes as hexadecimal string.

    Args:
        data: The byte data to encode.
        uppercase: If True, use uppercase hex digits (A-F).

    Returns None on error.
    All computation performed in formally verified Idris 2 code.
    """
    var pair = _str_to_ptr(data)
    var result = proven_hex_encode(pair[0], pair[1], uppercase)
    return string_result_to_string(result)
