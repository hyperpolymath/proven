# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

# cython: language_level=3

"""
Proven - Code that cannot crash.

Cython bindings for the libproven formally verified safety library.
All computation delegates to the Idris 2 + Zig FFI layer via direct
C function calls. This binding NEVER reimplements logic.

Modules:
    safe_math     - Arithmetic without overflow/underflow/division-by-zero
    safe_string   - UTF-8 validation and escaping
    safe_path     - Filesystem traversal prevention
    safe_email    - Email validation (RFC 5321)
    safe_url      - URL parsing and validation
    safe_crypto   - Constant-time comparison, secure random bytes
    safe_json     - JSON validation and type detection
    safe_datetime - ISO 8601 date/time handling
"""

cimport proven.lib_proven as c


# Status constants re-exported for user convenience
PROVEN_OK                    = c.PROVEN_OK
PROVEN_ERR_NULL_POINTER      = c.PROVEN_ERR_NULL_POINTER
PROVEN_ERR_INVALID_ARGUMENT  = c.PROVEN_ERR_INVALID_ARGUMENT
PROVEN_ERR_OVERFLOW          = c.PROVEN_ERR_OVERFLOW
PROVEN_ERR_UNDERFLOW         = c.PROVEN_ERR_UNDERFLOW
PROVEN_ERR_DIVISION_BY_ZERO  = c.PROVEN_ERR_DIVISION_BY_ZERO
PROVEN_ERR_PARSE_FAILURE     = c.PROVEN_ERR_PARSE_FAILURE
PROVEN_ERR_VALIDATION_FAILED = c.PROVEN_ERR_VALIDATION_FAILED
PROVEN_ERR_OUT_OF_BOUNDS     = c.PROVEN_ERR_OUT_OF_BOUNDS
PROVEN_ERR_ENCODING_ERROR    = c.PROVEN_ERR_ENCODING_ERROR
PROVEN_ERR_ALLOCATION_FAILED = c.PROVEN_ERR_ALLOCATION_FAILED
PROVEN_ERR_NOT_IMPLEMENTED   = c.PROVEN_ERR_NOT_IMPLEMENTED

__version__ = "1.0.0"


def init():
    """Initialize the Proven runtime (includes Idris 2 runtime).
    Must be called before any other Proven function. Safe to call multiple times.
    Returns 0 on success.
    """
    return c.proven_init()


def deinit():
    """Cleanup the Proven runtime. Call when done using Proven functions."""
    c.proven_deinit()


def is_initialized():
    """Check if the runtime is initialized. Returns True if initialized."""
    return bool(c.proven_is_initialized())


def version_major():
    """Get major version number."""
    return c.proven_version_major()


def version_minor():
    """Get minor version number."""
    return c.proven_version_minor()


def version_patch():
    """Get patch version number."""
    return c.proven_version_patch()


def module_count():
    """Get total module count."""
    return c.proven_module_count()


def abi_version():
    """Get FFI ABI version for compatibility checking."""
    return c.proven_ffi_abi_version()
