# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeEmail -- RFC 5321 email address validation.

Validates email addresses according to a simplified RFC 5321 grammar.
All computation is delegated to the formally verified Idris 2 core via C FFI.
No logic is reimplemented here.

Functions:
    is_valid_email -- Validate an email address string
"""

from memory import UnsafePointer

from .lib_proven import (
    BoolResult,
    proven_email_is_valid,
    PROVEN_OK,
)


fn _str_to_ptr(s: String) -> (UnsafePointer[UInt8], Int):
    """Extract a raw byte pointer and length from a Mojo String."""
    var byte_slice = s.as_bytes()
    var length = len(byte_slice)
    var ptr = byte_slice.unsafe_ptr()
    return (ptr, length)


fn is_valid_email(email: String) -> Bool:
    """Validate an email address according to RFC 5321 (simplified).

    Returns True if the email address is syntactically valid, False otherwise.
    On error, returns False as a safe default.
    All computation performed in formally verified Idris 2 code.
    """
    var pair = _str_to_ptr(email)
    var result = proven_email_is_valid(pair[0], pair[1])
    if result.succeeded():
        return result.value
    return False
