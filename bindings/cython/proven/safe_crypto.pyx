# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

# cython: language_level=3

"""
SafeCrypto - Cryptographic primitives.

All functions delegate to libproven via direct C FFI calls.
Returns None on error. NEVER reimplements logic.
"""

from libc.stdint cimport uint8_t
from libc.stddef cimport size_t
from libc.stdlib cimport malloc, free

cimport proven.lib_proven as c


def constant_time_eq(a, b):
    """Constant-time byte comparison (timing-attack safe).
    Arguments can be bytes or str. Returns True/False, or None on error.

    >>> constant_time_eq(b"secret", b"secret")
    True
    >>> constant_time_eq(b"aaa", b"bbb")
    False
    """
    cdef bytes ba, bb
    if isinstance(a, str):
        ba = a.encode("utf-8")
    else:
        ba = bytes(a)
    if isinstance(b, str):
        bb = b.encode("utf-8")
    else:
        bb = bytes(b)

    cdef const uint8_t* ptr1 = <const uint8_t*><char*>ba
    cdef size_t len1 = len(ba)
    cdef const uint8_t* ptr2 = <const uint8_t*><char*>bb
    cdef size_t len2 = len(bb)
    cdef c.ProvenBoolResult result
    with nogil:
        result = c.proven_crypto_constant_time_eq(ptr1, len1, ptr2, len2)
    if result.status == c.PROVEN_OK:
        return bool(result.value)
    return None


def random_bytes(size_t n):
    """Generate n cryptographically secure random bytes.
    Returns bytes object, or None on error.

    >>> len(random_bytes(32))
    32
    """
    if n == 0:
        return None
    cdef uint8_t* buf = <uint8_t*>malloc(n)
    if buf == NULL:
        return None
    cdef c.ProvenStatus status
    with nogil:
        status = c.proven_crypto_random_bytes(buf, n)
    if status == c.PROVEN_OK:
        result = buf[:n]
        free(buf)
        return result
    free(buf)
    return None
