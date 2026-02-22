# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

# cython: language_level=3

"""
SafeString - Text operations that handle encoding safely.

All functions delegate to libproven via direct C FFI calls.
Returns None on error. NEVER reimplements logic.
"""

from libc.stdint cimport uint8_t
from libc.stddef cimport size_t

cimport proven.lib_proven as c


cdef _decode_string_result(c.ProvenStringResult result):
    """Extract a Python string from a StringResult, freeing the C memory.
    Returns None if the result status is not OK.
    """
    if result.status != c.PROVEN_OK:
        return None
    if result.value == NULL:
        return None
    cdef bytes py_bytes = result.value[:result.length]
    c.proven_free_string(result.value)
    return py_bytes.decode("utf-8", errors="replace")


def is_valid_utf8(data):
    """Check if byte data is valid UTF-8. Returns True/False, or None on error.

    >>> is_valid_utf8(b"hello")
    True
    """
    cdef bytes b
    if isinstance(data, str):
        b = data.encode("utf-8")
    else:
        b = bytes(data)
    cdef const uint8_t* ptr = <const uint8_t*><char*>b
    cdef size_t length = len(b)
    cdef c.ProvenBoolResult result
    with nogil:
        result = c.proven_string_is_valid_utf8(ptr, length)
    if result.status == c.PROVEN_OK:
        return bool(result.value)
    return None


def escape_sql(str s):
    """Escape a string for SQL (single quotes). Returns None on error.
    Prefer parameterized queries over string escaping.

    >>> escape_sql("O'Brien")
    "O''Brien"
    """
    cdef bytes b = s.encode("utf-8")
    cdef const uint8_t* ptr = <const uint8_t*><char*>b
    cdef size_t length = len(b)
    cdef c.ProvenStringResult result
    with nogil:
        result = c.proven_string_escape_sql(ptr, length)
    return _decode_string_result(result)


def escape_html(str s):
    """Escape a string for HTML (prevents XSS). Returns None on error.

    >>> escape_html("<script>")
    "&lt;script&gt;"
    """
    cdef bytes b = s.encode("utf-8")
    cdef const uint8_t* ptr = <const uint8_t*><char*>b
    cdef size_t length = len(b)
    cdef c.ProvenStringResult result
    with nogil:
        result = c.proven_string_escape_html(ptr, length)
    return _decode_string_result(result)


def escape_js(str s):
    """Escape a string for JavaScript string literals. Returns None on error.

    >>> escape_js("alert('xss')")
    """
    cdef bytes b = s.encode("utf-8")
    cdef const uint8_t* ptr = <const uint8_t*><char*>b
    cdef size_t length = len(b)
    cdef c.ProvenStringResult result
    with nogil:
        result = c.proven_string_escape_js(ptr, length)
    return _decode_string_result(result)
