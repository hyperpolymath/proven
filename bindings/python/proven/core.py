# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
Core types and FFI loading for Proven.
"""

import ctypes
import ctypes.util
import os
import sys
from enum import IntEnum
from pathlib import Path
from typing import Optional


class ProvenStatus(IntEnum):
    """Status codes from Proven FFI operations."""
    OK = 0
    ERR_NULL_POINTER = -1
    ERR_INVALID_ARGUMENT = -2
    ERR_OVERFLOW = -3
    ERR_UNDERFLOW = -4
    ERR_DIVISION_BY_ZERO = -5
    ERR_PARSE_FAILURE = -6
    ERR_VALIDATION_FAILED = -7
    ERR_OUT_OF_BOUNDS = -8
    ERR_ENCODING_ERROR = -9
    ERR_ALLOCATION_FAILED = -10
    ERR_NOT_IMPLEMENTED = -99


class ProvenError(Exception):
    """Exception raised when a Proven operation fails."""

    def __init__(self, status: ProvenStatus, message: str = ""):
        self.status = status
        self.message = message or self._default_message(status)
        super().__init__(self.message)

    @staticmethod
    def _default_message(status: ProvenStatus) -> str:
        messages = {
            ProvenStatus.ERR_NULL_POINTER: "Null pointer passed to function",
            ProvenStatus.ERR_INVALID_ARGUMENT: "Invalid argument",
            ProvenStatus.ERR_OVERFLOW: "Integer overflow",
            ProvenStatus.ERR_UNDERFLOW: "Integer underflow",
            ProvenStatus.ERR_DIVISION_BY_ZERO: "Division by zero",
            ProvenStatus.ERR_PARSE_FAILURE: "Parse failure",
            ProvenStatus.ERR_VALIDATION_FAILED: "Validation failed",
            ProvenStatus.ERR_OUT_OF_BOUNDS: "Index out of bounds",
            ProvenStatus.ERR_ENCODING_ERROR: "Encoding error",
            ProvenStatus.ERR_ALLOCATION_FAILED: "Memory allocation failed",
            ProvenStatus.ERR_NOT_IMPLEMENTED: "Not implemented",
        }
        return messages.get(status, f"Unknown error: {status}")


# FFI result structures
class IntResult(ctypes.Structure):
    """Result structure for integer operations."""
    _fields_ = [
        ("status", ctypes.c_int),
        ("value", ctypes.c_int64),
    ]


class BoolResult(ctypes.Structure):
    """Result structure for boolean operations."""
    _fields_ = [
        ("status", ctypes.c_int),
        ("value", ctypes.c_bool),
    ]


class StringResult(ctypes.Structure):
    """Result structure for string operations."""
    _fields_ = [
        ("status", ctypes.c_int),
        ("value", ctypes.c_char_p),
        ("length", ctypes.c_size_t),
    ]


class UrlComponents(ctypes.Structure):
    """Parsed URL components."""
    _fields_ = [
        ("scheme", ctypes.c_char_p),
        ("scheme_len", ctypes.c_size_t),
        ("host", ctypes.c_char_p),
        ("host_len", ctypes.c_size_t),
        ("port", ctypes.c_uint16),
        ("has_port", ctypes.c_bool),
        ("path", ctypes.c_char_p),
        ("path_len", ctypes.c_size_t),
        ("query", ctypes.c_char_p),
        ("query_len", ctypes.c_size_t),
        ("fragment", ctypes.c_char_p),
        ("fragment_len", ctypes.c_size_t),
    ]


class UrlResult(ctypes.Structure):
    """Result structure for URL parsing."""
    _fields_ = [
        ("status", ctypes.c_int),
        ("components", UrlComponents),
    ]


class IPv4Address(ctypes.Structure):
    """IPv4 address structure."""
    _fields_ = [
        ("octets", ctypes.c_uint8 * 4),
    ]


class IPv4Result(ctypes.Structure):
    """Result structure for IPv4 parsing."""
    _fields_ = [
        ("status", ctypes.c_int),
        ("address", IPv4Address),
    ]


def _find_library() -> Optional[str]:
    """Find the proven shared library."""
    # Check common locations
    search_paths = [
        # Relative to this file (for development)
        Path(__file__).parent.parent.parent.parent / "ffi" / "zig" / "zig-out" / "lib",
        # System paths
        Path("/usr/local/lib"),
        Path("/usr/lib"),
        # User local
        Path.home() / ".local" / "lib",
    ]

    lib_names = ["libproven.so", "libproven.dylib", "proven.dll"]

    for search_path in search_paths:
        for lib_name in lib_names:
            lib_path = search_path / lib_name
            if lib_path.exists():
                return str(lib_path)

    # Try system library finder
    return ctypes.util.find_library("proven")


def _load_library() -> ctypes.CDLL:
    """Load the proven shared library."""
    lib_path = _find_library()

    if lib_path is None:
        # For now, return a mock that raises NotImplementedError
        # This allows the package to be imported even without the native library
        raise ImportError(
            "Could not find libproven shared library. "
            "Please build it with: cd ffi/zig && zig build"
        )

    lib = ctypes.CDLL(lib_path)
    _setup_function_signatures(lib)
    return lib


def _setup_function_signatures(lib: ctypes.CDLL) -> None:
    """Set up ctypes function signatures for type safety."""

    # Memory management
    lib.proven_free_string.argtypes = [ctypes.c_char_p]
    lib.proven_free_string.restype = None

    # SafeMath
    lib.proven_math_div.argtypes = [ctypes.c_int64, ctypes.c_int64]
    lib.proven_math_div.restype = IntResult

    lib.proven_math_mod.argtypes = [ctypes.c_int64, ctypes.c_int64]
    lib.proven_math_mod.restype = IntResult

    lib.proven_math_add_checked.argtypes = [ctypes.c_int64, ctypes.c_int64]
    lib.proven_math_add_checked.restype = IntResult

    lib.proven_math_sub_checked.argtypes = [ctypes.c_int64, ctypes.c_int64]
    lib.proven_math_sub_checked.restype = IntResult

    lib.proven_math_mul_checked.argtypes = [ctypes.c_int64, ctypes.c_int64]
    lib.proven_math_mul_checked.restype = IntResult

    lib.proven_math_abs_safe.argtypes = [ctypes.c_int64]
    lib.proven_math_abs_safe.restype = IntResult

    lib.proven_math_clamp.argtypes = [ctypes.c_int64, ctypes.c_int64, ctypes.c_int64]
    lib.proven_math_clamp.restype = ctypes.c_int64

    lib.proven_math_pow_checked.argtypes = [ctypes.c_int64, ctypes.c_uint32]
    lib.proven_math_pow_checked.restype = IntResult

    # SafeString
    lib.proven_string_is_valid_utf8.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_string_is_valid_utf8.restype = BoolResult

    lib.proven_string_escape_sql.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_string_escape_sql.restype = StringResult

    lib.proven_string_escape_html.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_string_escape_html.restype = StringResult

    lib.proven_string_escape_js.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_string_escape_js.restype = StringResult

    # SafePath
    lib.proven_path_has_traversal.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_path_has_traversal.restype = BoolResult

    lib.proven_path_sanitize_filename.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_path_sanitize_filename.restype = StringResult

    # SafeCrypto
    lib.proven_crypto_constant_time_eq.argtypes = [
        ctypes.c_char_p, ctypes.c_size_t,
        ctypes.c_char_p, ctypes.c_size_t
    ]
    lib.proven_crypto_constant_time_eq.restype = BoolResult

    lib.proven_crypto_random_bytes.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_crypto_random_bytes.restype = ctypes.c_int

    # SafeUrl
    lib.proven_url_parse.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_url_parse.restype = UrlResult

    lib.proven_url_free.argtypes = [ctypes.POINTER(UrlComponents)]
    lib.proven_url_free.restype = None

    # SafeEmail
    lib.proven_email_is_valid.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_email_is_valid.restype = BoolResult

    # SafeNetwork
    lib.proven_network_parse_ipv4.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_network_parse_ipv4.restype = IPv4Result

    lib.proven_network_ipv4_is_private.argtypes = [IPv4Address]
    lib.proven_network_ipv4_is_private.restype = ctypes.c_bool

    lib.proven_network_ipv4_is_loopback.argtypes = [IPv4Address]
    lib.proven_network_ipv4_is_loopback.restype = ctypes.c_bool

    # Version
    lib.proven_version_major.argtypes = []
    lib.proven_version_major.restype = ctypes.c_uint32

    lib.proven_version_minor.argtypes = []
    lib.proven_version_minor.restype = ctypes.c_uint32

    lib.proven_version_patch.argtypes = []
    lib.proven_version_patch.restype = ctypes.c_uint32


# Lazy loading of the library
_lib: Optional[ctypes.CDLL] = None


def get_lib() -> ctypes.CDLL:
    """Get the loaded library, initializing if necessary."""
    global _lib
    if _lib is None:
        _lib = _load_library()
    return _lib


def check_status(status: int) -> None:
    """Check status code and raise ProvenError if not OK."""
    if status != ProvenStatus.OK:
        raise ProvenError(ProvenStatus(status))
