# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafePath -- Filesystem path traversal prevention.

Detects directory traversal attacks (..) and sanitizes filenames by removing
dangerous characters. All computation is delegated to the formally verified
Idris 2 core via C FFI. No logic is reimplemented here.

Functions:
    has_traversal      -- Check if path contains directory traversal sequences
    sanitize_filename  -- Remove dangerous characters from a filename
"""

from memory import UnsafePointer

from .lib_proven import (
    BoolResult,
    StringResult,
    proven_path_has_traversal,
    proven_path_sanitize_filename,
    proven_free_string,
    string_result_to_string,
    PROVEN_OK,
)


fn _str_to_ptr(s: String) -> (UnsafePointer[UInt8], Int):
    """Extract a raw byte pointer and length from a Mojo String."""
    var byte_slice = s.as_bytes()
    var length = len(byte_slice)
    var ptr = byte_slice.unsafe_ptr()
    return (ptr, length)


fn has_traversal(path: String) -> Bool:
    """Check if a path contains directory traversal sequences ('..').

    Returns True if traversal is detected, False if the path is safe.
    On error (e.g. null pointer), returns True as a safe default.
    All computation performed in formally verified Idris 2 code.
    """
    var pair = _str_to_ptr(path)
    var result = proven_path_has_traversal(pair[0], pair[1])
    if result.succeeded():
        return result.value
    # On error, assume traversal is present (safe default)
    return True


fn sanitize_filename(filename: String) -> Optional[String]:
    """Sanitize a filename by removing dangerous characters.

    Strips path separators, null bytes, and other dangerous characters
    from the filename. Returns None on error.
    All computation performed in formally verified Idris 2 code.
    """
    var pair = _str_to_ptr(filename)
    var result = proven_path_sanitize_filename(pair[0], pair[1])
    return string_result_to_string(result)
