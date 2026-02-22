# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

# cython: language_level=3

"""
SafePath - Filesystem traversal prevention.

All functions delegate to libproven via direct C FFI calls.
Returns None on error. NEVER reimplements logic.
"""

from libc.stdint cimport uint8_t
from libc.stddef cimport size_t

cimport proven.lib_proven as c


cdef _decode_string_result(c.ProvenStringResult result):
    """Extract a Python string from a StringResult, freeing the C memory."""
    if result.status != c.PROVEN_OK:
        return None
    if result.value == NULL:
        return None
    cdef bytes py_bytes = result.value[:result.length]
    c.proven_free_string(result.value)
    return py_bytes.decode("utf-8", errors="replace")


def has_traversal(str path_str):
    """Check if path contains directory traversal sequences ('..').
    Returns True if traversal detected, False if safe, None on error.

    >>> has_traversal("../etc/passwd")
    True
    >>> has_traversal("safe/file.txt")
    False
    """
    cdef bytes b = path_str.encode("utf-8")
    cdef const uint8_t* ptr = <const uint8_t*><char*>b
    cdef size_t length = len(b)
    cdef c.ProvenBoolResult result
    with nogil:
        result = c.proven_path_has_traversal(ptr, length)
    if result.status == c.PROVEN_OK:
        return bool(result.value)
    return None


def sanitize_filename(str filename):
    """Sanitize a filename by removing dangerous characters. Returns None on error.

    >>> sanitize_filename("../../etc/passwd")
    """
    cdef bytes b = filename.encode("utf-8")
    cdef const uint8_t* ptr = <const uint8_t*><char*>b
    cdef size_t length = len(b)
    cdef c.ProvenStringResult result
    with nogil:
        result = c.proven_path_sanitize_filename(ptr, length)
    return _decode_string_result(result)


def is_safe_path(str path_str):
    """Convenience: returns True if path has no traversal. Returns None on error.

    >>> is_safe_path("data/file.txt")
    True
    >>> is_safe_path("../../../etc/shadow")
    False
    """
    traversal = has_traversal(path_str)
    if traversal is None:
        return None
    return not traversal
