# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
Core types and FFI loading for Proven.

Provides the ctypes FFI bridge to libproven (Idris 2 via Zig).
All computation is delegated to the formally verified Idris core.
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


class DoubleResult(ctypes.Structure):
    """Result structure for floating-point operations."""
    _fields_ = [
        ("status", ctypes.c_int),
        ("value", ctypes.c_double),
    ]


class UInt32Result(ctypes.Structure):
    """Result structure for unsigned 32-bit integer operations."""
    _fields_ = [
        ("status", ctypes.c_int),
        ("value", ctypes.c_uint32),
    ]


class SizeResult(ctypes.Structure):
    """Result structure for size_t operations."""
    _fields_ = [
        ("status", ctypes.c_int),
        ("value", ctypes.c_size_t),
    ]


class BytesResult(ctypes.Structure):
    """Result structure for byte buffer operations."""
    _fields_ = [
        ("status", ctypes.c_int),
        ("data", ctypes.c_char_p),
        ("length", ctypes.c_size_t),
    ]


class HandleResult(ctypes.Structure):
    """Result structure for opaque handle operations."""
    _fields_ = [
        ("status", ctypes.c_int),
        ("handle", ctypes.c_void_p),
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

    # SafeJson
    lib.proven_json_parse.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_json_parse.restype = StringResult

    lib.proven_json_stringify.argtypes = [ctypes.c_char_p, ctypes.c_size_t, ctypes.c_bool]
    lib.proven_json_stringify.restype = StringResult

    lib.proven_json_get_string.argtypes = [ctypes.c_char_p, ctypes.c_size_t,
                                           ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_json_get_string.restype = StringResult

    lib.proven_json_get_int.argtypes = [ctypes.c_char_p, ctypes.c_size_t,
                                        ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_json_get_int.restype = IntResult

    lib.proven_json_get_bool.argtypes = [ctypes.c_char_p, ctypes.c_size_t,
                                         ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_json_get_bool.restype = BoolResult

    # SafeUUID
    lib.proven_uuid_v4.argtypes = [ctypes.c_char_p]  # 16-byte output buffer
    lib.proven_uuid_v4.restype = ctypes.c_int

    lib.proven_uuid_v5.argtypes = [ctypes.c_char_p, ctypes.c_size_t,  # namespace (16 bytes)
                                   ctypes.c_char_p, ctypes.c_size_t,  # name
                                   ctypes.c_char_p]  # 16-byte output buffer
    lib.proven_uuid_v5.restype = ctypes.c_int

    lib.proven_uuid_parse.argtypes = [ctypes.c_char_p, ctypes.c_size_t,
                                      ctypes.c_char_p]  # 16-byte output buffer
    lib.proven_uuid_parse.restype = ctypes.c_int

    lib.proven_uuid_format.argtypes = [ctypes.c_char_p, ctypes.c_size_t,  # 16 bytes in
                                       ctypes.c_char_p, ctypes.c_size_t]  # output buffer
    lib.proven_uuid_format.restype = StringResult

    # SafeHex
    lib.proven_hex_encode.argtypes = [ctypes.c_char_p, ctypes.c_size_t, ctypes.c_bool]
    lib.proven_hex_encode.restype = StringResult

    lib.proven_hex_decode.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_hex_decode.restype = BytesResult

    # SafePhone
    lib.proven_phone_parse.argtypes = [ctypes.c_char_p, ctypes.c_size_t,
                                       ctypes.c_char_p, ctypes.c_size_t]  # default country
    lib.proven_phone_parse.restype = StringResult  # Returns E.164 string

    lib.proven_phone_is_valid.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_phone_is_valid.restype = BoolResult

    lib.proven_phone_format_e164.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_phone_format_e164.restype = StringResult

    lib.proven_phone_format_international.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_phone_format_international.restype = StringResult

    lib.proven_phone_format_national.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_phone_format_national.restype = StringResult

    lib.proven_phone_get_country_code.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_phone_get_country_code.restype = StringResult

    # SafePassword
    lib.proven_password_validate.argtypes = [ctypes.c_char_p, ctypes.c_size_t,
                                             ctypes.c_int,   # min_length
                                             ctypes.c_int,   # max_length
                                             ctypes.c_bool,  # require_upper
                                             ctypes.c_bool,  # require_lower
                                             ctypes.c_bool,  # require_digit
                                             ctypes.c_bool]  # require_special
    lib.proven_password_validate.restype = StringResult  # JSON result

    lib.proven_password_entropy.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_password_entropy.restype = DoubleResult

    lib.proven_password_is_common.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_password_is_common.restype = BoolResult

    lib.proven_password_has_sequential.argtypes = [ctypes.c_char_p, ctypes.c_size_t, ctypes.c_int]
    lib.proven_password_has_sequential.restype = BoolResult

    lib.proven_password_has_repeated.argtypes = [ctypes.c_char_p, ctypes.c_size_t, ctypes.c_int]
    lib.proven_password_has_repeated.restype = BoolResult

    # SafeDateTime
    lib.proven_datetime_parse_iso8601.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_datetime_parse_iso8601.restype = StringResult  # Returns normalized ISO string

    lib.proven_datetime_parse_date.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_datetime_parse_date.restype = StringResult

    lib.proven_datetime_parse_time.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_datetime_parse_time.restype = StringResult

    lib.proven_datetime_is_leap_year.argtypes = [ctypes.c_int]
    lib.proven_datetime_is_leap_year.restype = BoolResult

    lib.proven_datetime_days_in_month.argtypes = [ctypes.c_int, ctypes.c_int]
    lib.proven_datetime_days_in_month.restype = IntResult

    # SafeHeader
    lib.proven_header_validate_name.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_header_validate_name.restype = BoolResult

    lib.proven_header_validate_value.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_header_validate_value.restype = BoolResult

    lib.proven_header_sanitize_value.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_header_sanitize_value.restype = StringResult

    lib.proven_header_parse.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_header_parse.restype = StringResult  # Returns "name\0value"

    # SafeCookie
    lib.proven_cookie_validate_name.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_cookie_validate_name.restype = BoolResult

    lib.proven_cookie_validate_value.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_cookie_validate_value.restype = BoolResult

    lib.proven_cookie_sanitize_value.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_cookie_sanitize_value.restype = StringResult

    lib.proven_cookie_parse_header.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_cookie_parse_header.restype = StringResult  # Returns JSON array

    # SafeContentType
    lib.proven_content_type_parse.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_content_type_parse.restype = StringResult  # Returns JSON object

    lib.proven_content_type_is_dangerous.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_content_type_is_dangerous.restype = BoolResult

    lib.proven_content_type_guess_ext.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_content_type_guess_ext.restype = StringResult

    # SafeFloat
    lib.proven_float_div.argtypes = [ctypes.c_double, ctypes.c_double]
    lib.proven_float_div.restype = DoubleResult

    lib.proven_float_add.argtypes = [ctypes.c_double, ctypes.c_double]
    lib.proven_float_add.restype = DoubleResult

    lib.proven_float_sub.argtypes = [ctypes.c_double, ctypes.c_double]
    lib.proven_float_sub.restype = DoubleResult

    lib.proven_float_mul.argtypes = [ctypes.c_double, ctypes.c_double]
    lib.proven_float_mul.restype = DoubleResult

    lib.proven_float_sqrt.argtypes = [ctypes.c_double]
    lib.proven_float_sqrt.restype = DoubleResult

    lib.proven_float_log.argtypes = [ctypes.c_double]
    lib.proven_float_log.restype = DoubleResult

    lib.proven_float_log10.argtypes = [ctypes.c_double]
    lib.proven_float_log10.restype = DoubleResult

    lib.proven_float_pow.argtypes = [ctypes.c_double, ctypes.c_double]
    lib.proven_float_pow.restype = DoubleResult

    lib.proven_float_is_finite.argtypes = [ctypes.c_double]
    lib.proven_float_is_finite.restype = BoolResult

    lib.proven_float_clamp.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
    lib.proven_float_clamp.restype = DoubleResult

    lib.proven_float_lerp.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
    lib.proven_float_lerp.restype = DoubleResult

    lib.proven_float_approx_eq.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
    lib.proven_float_approx_eq.restype = BoolResult

    # SafeTensor (vectors and matrices via flat arrays)
    lib.proven_tensor_dot.argtypes = [ctypes.POINTER(ctypes.c_double), ctypes.c_size_t,
                                      ctypes.POINTER(ctypes.c_double), ctypes.c_size_t]
    lib.proven_tensor_dot.restype = DoubleResult

    lib.proven_tensor_magnitude.argtypes = [ctypes.POINTER(ctypes.c_double), ctypes.c_size_t]
    lib.proven_tensor_magnitude.restype = DoubleResult

    lib.proven_tensor_normalize.argtypes = [ctypes.POINTER(ctypes.c_double), ctypes.c_size_t,
                                            ctypes.POINTER(ctypes.c_double)]  # output
    lib.proven_tensor_normalize.restype = ctypes.c_int

    lib.proven_tensor_add.argtypes = [ctypes.POINTER(ctypes.c_double), ctypes.c_size_t,
                                      ctypes.POINTER(ctypes.c_double), ctypes.c_size_t,
                                      ctypes.POINTER(ctypes.c_double)]  # output
    lib.proven_tensor_add.restype = ctypes.c_int

    lib.proven_tensor_sub.argtypes = [ctypes.POINTER(ctypes.c_double), ctypes.c_size_t,
                                      ctypes.POINTER(ctypes.c_double), ctypes.c_size_t,
                                      ctypes.POINTER(ctypes.c_double)]  # output
    lib.proven_tensor_sub.restype = ctypes.c_int

    lib.proven_tensor_scale.argtypes = [ctypes.POINTER(ctypes.c_double), ctypes.c_size_t,
                                        ctypes.c_double,
                                        ctypes.POINTER(ctypes.c_double)]  # output
    lib.proven_tensor_scale.restype = ctypes.c_int

    lib.proven_tensor_matmul.argtypes = [ctypes.POINTER(ctypes.c_double),
                                         ctypes.c_size_t, ctypes.c_size_t,  # a rows, cols
                                         ctypes.POINTER(ctypes.c_double),
                                         ctypes.c_size_t, ctypes.c_size_t,  # b rows, cols
                                         ctypes.POINTER(ctypes.c_double)]   # output
    lib.proven_tensor_matmul.restype = ctypes.c_int

    # SafeML
    lib.proven_ml_softmax.argtypes = [ctypes.POINTER(ctypes.c_double), ctypes.c_size_t,
                                      ctypes.POINTER(ctypes.c_double)]  # output
    lib.proven_ml_softmax.restype = ctypes.c_int

    lib.proven_ml_sigmoid.argtypes = [ctypes.c_double]
    lib.proven_ml_sigmoid.restype = DoubleResult

    lib.proven_ml_relu.argtypes = [ctypes.c_double]
    lib.proven_ml_relu.restype = DoubleResult

    lib.proven_ml_tanh.argtypes = [ctypes.c_double]
    lib.proven_ml_tanh.restype = DoubleResult

    lib.proven_ml_cross_entropy.argtypes = [ctypes.POINTER(ctypes.c_double), ctypes.c_size_t,
                                            ctypes.c_int]
    lib.proven_ml_cross_entropy.restype = DoubleResult

    lib.proven_ml_mse.argtypes = [ctypes.POINTER(ctypes.c_double), ctypes.c_size_t,
                                  ctypes.POINTER(ctypes.c_double), ctypes.c_size_t]
    lib.proven_ml_mse.restype = DoubleResult

    lib.proven_ml_batch_norm.argtypes = [ctypes.POINTER(ctypes.c_double), ctypes.c_size_t,
                                         ctypes.c_double,  # epsilon
                                         ctypes.POINTER(ctypes.c_double)]  # output
    lib.proven_ml_batch_norm.restype = ctypes.c_int

    # SafeCalculator
    lib.proven_calculator_evaluate.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_calculator_evaluate.restype = DoubleResult

    # SafeVersion
    lib.proven_version_parse_semver.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_version_parse_semver.restype = StringResult  # Returns JSON

    lib.proven_version_compare.argtypes = [ctypes.c_char_p, ctypes.c_size_t,
                                           ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_version_compare.restype = IntResult

    lib.proven_version_satisfies.argtypes = [ctypes.c_char_p, ctypes.c_size_t,
                                             ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_version_satisfies.restype = BoolResult

    # SafeGeo
    lib.proven_geo_haversine.argtypes = [ctypes.c_double, ctypes.c_double,
                                         ctypes.c_double, ctypes.c_double]
    lib.proven_geo_haversine.restype = DoubleResult

    lib.proven_geo_bearing.argtypes = [ctypes.c_double, ctypes.c_double,
                                       ctypes.c_double, ctypes.c_double]
    lib.proven_geo_bearing.restype = DoubleResult

    lib.proven_geo_destination.argtypes = [ctypes.c_double, ctypes.c_double,
                                           ctypes.c_double, ctypes.c_double,
                                           ctypes.POINTER(ctypes.c_double),  # out_lat
                                           ctypes.POINTER(ctypes.c_double)]  # out_lon
    lib.proven_geo_destination.restype = ctypes.c_int

    lib.proven_geo_validate_coord.argtypes = [ctypes.c_double, ctypes.c_double]
    lib.proven_geo_validate_coord.restype = BoolResult

    # SafeAngle
    lib.proven_angle_deg_to_rad.argtypes = [ctypes.c_double]
    lib.proven_angle_deg_to_rad.restype = DoubleResult

    lib.proven_angle_rad_to_deg.argtypes = [ctypes.c_double]
    lib.proven_angle_rad_to_deg.restype = DoubleResult

    lib.proven_angle_normalize_deg.argtypes = [ctypes.c_double]
    lib.proven_angle_normalize_deg.restype = DoubleResult

    lib.proven_angle_normalize_rad.argtypes = [ctypes.c_double]
    lib.proven_angle_normalize_rad.restype = DoubleResult

    lib.proven_angle_sin.argtypes = [ctypes.c_double]
    lib.proven_angle_sin.restype = DoubleResult

    lib.proven_angle_cos.argtypes = [ctypes.c_double]
    lib.proven_angle_cos.restype = DoubleResult

    lib.proven_angle_tan.argtypes = [ctypes.c_double]
    lib.proven_angle_tan.restype = DoubleResult

    # SafeChecksum
    lib.proven_checksum_crc32.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_checksum_crc32.restype = UInt32Result

    lib.proven_checksum_adler32.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_checksum_adler32.restype = UInt32Result

    lib.proven_checksum_fnv1a_64.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_checksum_fnv1a_64.restype = IntResult  # 64-bit

    lib.proven_checksum_fnv1a_32.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_checksum_fnv1a_32.restype = UInt32Result

    lib.proven_checksum_luhn_check.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_checksum_luhn_check.restype = BoolResult

    # SafeProbability
    lib.proven_probability_clamp.argtypes = [ctypes.c_double]
    lib.proven_probability_clamp.restype = DoubleResult

    lib.proven_probability_and.argtypes = [ctypes.c_double, ctypes.c_double]
    lib.proven_probability_and.restype = DoubleResult

    lib.proven_probability_or.argtypes = [ctypes.c_double, ctypes.c_double]
    lib.proven_probability_or.restype = DoubleResult

    lib.proven_probability_not.argtypes = [ctypes.c_double]
    lib.proven_probability_not.restype = DoubleResult

    lib.proven_probability_bayes.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
    lib.proven_probability_bayes.restype = DoubleResult

    lib.proven_probability_binomial.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_double]
    lib.proven_probability_binomial.restype = DoubleResult

    # SafeUnit
    lib.proven_unit_convert_length.argtypes = [ctypes.c_double, ctypes.c_int, ctypes.c_int]
    lib.proven_unit_convert_length.restype = DoubleResult

    lib.proven_unit_convert_mass.argtypes = [ctypes.c_double, ctypes.c_int, ctypes.c_int]
    lib.proven_unit_convert_mass.restype = DoubleResult

    lib.proven_unit_convert_temperature.argtypes = [ctypes.c_double, ctypes.c_int, ctypes.c_int]
    lib.proven_unit_convert_temperature.restype = DoubleResult

    lib.proven_unit_convert_time.argtypes = [ctypes.c_double, ctypes.c_int, ctypes.c_int]
    lib.proven_unit_convert_time.restype = DoubleResult

    lib.proven_unit_convert_data.argtypes = [ctypes.c_double, ctypes.c_int, ctypes.c_int]
    lib.proven_unit_convert_data.restype = DoubleResult

    # SafeColor
    lib.proven_color_parse_hex.argtypes = [ctypes.c_char_p, ctypes.c_size_t,
                                           ctypes.POINTER(ctypes.c_uint8)]  # 4-byte RGBA out
    lib.proven_color_parse_hex.restype = ctypes.c_int

    lib.proven_color_luminance.argtypes = [ctypes.c_uint8, ctypes.c_uint8, ctypes.c_uint8]
    lib.proven_color_luminance.restype = DoubleResult

    lib.proven_color_contrast_ratio.argtypes = [ctypes.c_uint8, ctypes.c_uint8, ctypes.c_uint8,
                                                ctypes.c_uint8, ctypes.c_uint8, ctypes.c_uint8]
    lib.proven_color_contrast_ratio.restype = DoubleResult

    lib.proven_color_blend.argtypes = [ctypes.c_uint8, ctypes.c_uint8, ctypes.c_uint8,
                                       ctypes.c_uint8,  # fg RGBA
                                       ctypes.c_uint8, ctypes.c_uint8, ctypes.c_uint8,  # bg RGB
                                       ctypes.POINTER(ctypes.c_uint8)]  # 3-byte RGB out
    lib.proven_color_blend.restype = ctypes.c_int

    # SafeBuffer
    lib.proven_buffer_create.argtypes = [ctypes.c_size_t]
    lib.proven_buffer_create.restype = HandleResult

    lib.proven_buffer_push.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_buffer_push.restype = BoolResult

    lib.proven_buffer_pop.argtypes = [ctypes.c_void_p]
    lib.proven_buffer_pop.restype = BytesResult

    lib.proven_buffer_len.argtypes = [ctypes.c_void_p]
    lib.proven_buffer_len.restype = SizeResult

    lib.proven_buffer_free.argtypes = [ctypes.c_void_p]
    lib.proven_buffer_free.restype = None

    # SafeQueue
    lib.proven_queue_create.argtypes = [ctypes.c_size_t]
    lib.proven_queue_create.restype = HandleResult

    lib.proven_queue_push.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_queue_push.restype = BoolResult

    lib.proven_queue_pop.argtypes = [ctypes.c_void_p]
    lib.proven_queue_pop.restype = BytesResult

    lib.proven_queue_len.argtypes = [ctypes.c_void_p]
    lib.proven_queue_len.restype = SizeResult

    lib.proven_queue_free.argtypes = [ctypes.c_void_p]
    lib.proven_queue_free.restype = None

    # SafeBloom
    lib.proven_bloom_create.argtypes = [ctypes.c_size_t, ctypes.c_double]
    lib.proven_bloom_create.restype = HandleResult

    lib.proven_bloom_insert.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_bloom_insert.restype = ctypes.c_int

    lib.proven_bloom_contains.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_bloom_contains.restype = BoolResult

    lib.proven_bloom_free.argtypes = [ctypes.c_void_p]
    lib.proven_bloom_free.restype = None

    # SafeLru
    lib.proven_lru_create.argtypes = [ctypes.c_size_t]
    lib.proven_lru_create.restype = HandleResult

    lib.proven_lru_get.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_lru_get.restype = BytesResult

    lib.proven_lru_put.argtypes = [ctypes.c_void_p,
                                   ctypes.c_char_p, ctypes.c_size_t,  # key
                                   ctypes.c_char_p, ctypes.c_size_t]  # value
    lib.proven_lru_put.restype = ctypes.c_int

    lib.proven_lru_remove.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_lru_remove.restype = ctypes.c_int

    lib.proven_lru_len.argtypes = [ctypes.c_void_p]
    lib.proven_lru_len.restype = SizeResult

    lib.proven_lru_free.argtypes = [ctypes.c_void_p]
    lib.proven_lru_free.restype = None

    # SafeRateLimiter
    lib.proven_ratelimit_token_bucket_create.argtypes = [ctypes.c_double, ctypes.c_size_t]
    lib.proven_ratelimit_token_bucket_create.restype = HandleResult

    lib.proven_ratelimit_token_bucket_acquire.argtypes = [ctypes.c_void_p, ctypes.c_size_t]
    lib.proven_ratelimit_token_bucket_acquire.restype = StringResult  # JSON result

    lib.proven_ratelimit_token_bucket_free.argtypes = [ctypes.c_void_p]
    lib.proven_ratelimit_token_bucket_free.restype = None

    lib.proven_ratelimit_sliding_window_create.argtypes = [ctypes.c_size_t, ctypes.c_double]
    lib.proven_ratelimit_sliding_window_create.restype = HandleResult

    lib.proven_ratelimit_sliding_window_acquire.argtypes = [ctypes.c_void_p]
    lib.proven_ratelimit_sliding_window_acquire.restype = StringResult

    lib.proven_ratelimit_sliding_window_free.argtypes = [ctypes.c_void_p]
    lib.proven_ratelimit_sliding_window_free.restype = None

    # SafeCircuitBreaker
    lib.proven_circuit_breaker_create.argtypes = [ctypes.c_int,    # failure_threshold
                                                  ctypes.c_int,    # success_threshold
                                                  ctypes.c_double, # timeout
                                                  ctypes.c_int]    # half_open_max
    lib.proven_circuit_breaker_create.restype = HandleResult

    lib.proven_circuit_breaker_allow.argtypes = [ctypes.c_void_p]
    lib.proven_circuit_breaker_allow.restype = BoolResult

    lib.proven_circuit_breaker_record_success.argtypes = [ctypes.c_void_p]
    lib.proven_circuit_breaker_record_success.restype = ctypes.c_int

    lib.proven_circuit_breaker_record_failure.argtypes = [ctypes.c_void_p]
    lib.proven_circuit_breaker_record_failure.restype = ctypes.c_int

    lib.proven_circuit_breaker_state.argtypes = [ctypes.c_void_p]
    lib.proven_circuit_breaker_state.restype = IntResult  # 0=closed, 1=open, 2=half_open

    lib.proven_circuit_breaker_free.argtypes = [ctypes.c_void_p]
    lib.proven_circuit_breaker_free.restype = None

    # SafeRetry
    lib.proven_retry_calculate_delay.argtypes = [ctypes.c_int,     # attempt
                                                 ctypes.c_double,  # base_delay
                                                 ctypes.c_double,  # multiplier
                                                 ctypes.c_double,  # max_delay
                                                 ctypes.c_double]  # jitter
    lib.proven_retry_calculate_delay.restype = DoubleResult

    # SafeMonotonic
    lib.proven_monotonic_counter_create.argtypes = [ctypes.c_int64]
    lib.proven_monotonic_counter_create.restype = HandleResult

    lib.proven_monotonic_counter_next.argtypes = [ctypes.c_void_p]
    lib.proven_monotonic_counter_next.restype = IntResult

    lib.proven_monotonic_counter_get.argtypes = [ctypes.c_void_p]
    lib.proven_monotonic_counter_get.restype = IntResult

    lib.proven_monotonic_counter_free.argtypes = [ctypes.c_void_p]
    lib.proven_monotonic_counter_free.restype = None

    # SafeStateMachine
    lib.proven_state_machine_create.argtypes = [ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_state_machine_create.restype = HandleResult

    lib.proven_state_machine_add_transition.argtypes = [ctypes.c_void_p,
                                                        ctypes.c_char_p, ctypes.c_size_t,
                                                        ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_state_machine_add_transition.restype = ctypes.c_int

    lib.proven_state_machine_transition.argtypes = [ctypes.c_void_p,
                                                    ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_state_machine_transition.restype = StringResult

    lib.proven_state_machine_current.argtypes = [ctypes.c_void_p]
    lib.proven_state_machine_current.restype = StringResult

    lib.proven_state_machine_can_transition.argtypes = [ctypes.c_void_p,
                                                        ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_state_machine_can_transition.restype = BoolResult

    lib.proven_state_machine_free.argtypes = [ctypes.c_void_p]
    lib.proven_state_machine_free.restype = None

    # SafeGraph
    lib.proven_graph_create.argtypes = []
    lib.proven_graph_create.restype = HandleResult

    lib.proven_graph_add_edge.argtypes = [ctypes.c_void_p,
                                          ctypes.c_char_p, ctypes.c_size_t,
                                          ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_graph_add_edge.restype = ctypes.c_int

    lib.proven_graph_has_cycle.argtypes = [ctypes.c_void_p]
    lib.proven_graph_has_cycle.restype = BoolResult

    lib.proven_graph_topological_sort.argtypes = [ctypes.c_void_p]
    lib.proven_graph_topological_sort.restype = StringResult  # JSON array

    lib.proven_graph_find_path.argtypes = [ctypes.c_void_p,
                                           ctypes.c_char_p, ctypes.c_size_t,
                                           ctypes.c_char_p, ctypes.c_size_t]
    lib.proven_graph_find_path.restype = StringResult  # JSON array

    lib.proven_graph_free.argtypes = [ctypes.c_void_p]
    lib.proven_graph_free.restype = None


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
