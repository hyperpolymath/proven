# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

# cython: language_level=3

"""
SafeEmail - Email address validation (RFC 5321 simplified).

All functions delegate to libproven via direct C FFI calls.
Returns None on error. NEVER reimplements logic.
"""

from libc.stdint cimport uint8_t
from libc.stddef cimport size_t

cimport proven.lib_proven as c


def is_valid_email(str email):
    """Validate an email address per RFC 5321 (simplified).
    Returns True if valid, False if invalid, None on FFI error.

    >>> is_valid_email("user@example.com")
    True
    >>> is_valid_email("not-an-email")
    False
    """
    cdef bytes b = email.encode("utf-8")
    cdef const uint8_t* ptr = <const uint8_t*><char*>b
    cdef size_t length = len(b)
    cdef c.ProvenBoolResult result
    with nogil:
        result = c.proven_email_is_valid(ptr, length)
    if result.status == c.PROVEN_OK:
        return bool(result.value)
    return None
