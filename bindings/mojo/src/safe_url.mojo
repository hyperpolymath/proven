# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeUrl -- URL parsing, validation, and HTTP encoding.

Parses URLs into components (scheme, host, port, path, query, fragment) and
provides URL percent-encoding/decoding per RFC 3986. All computation is
delegated to the formally verified Idris 2 core via C FFI. No logic is
reimplemented here.

Note: The full ProvenUrlResult/ProvenUrlComponents C struct involves heap-
allocated string members and a custom free function (proven_url_free). This
module uses the lower-level C FFI directly and copies data to Mojo-owned
strings before freeing the C allocation.

Functions:
    url_encode -- Percent-encode a string per RFC 3986
    url_decode -- Decode a percent-encoded string
"""

from memory import UnsafePointer

from .lib_proven import (
    StringResult,
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


# =============================================================================
# HTTP URL encoding FFI (from SafeHttp section in proven.h)
# =============================================================================


fn _proven_http_url_encode(
    ptr: UnsafePointer[UInt8], length: Int
) -> StringResult:
    """Raw FFI call to proven_http_url_encode."""
    return external_call[
        "proven_http_url_encode", StringResult, UnsafePointer[UInt8], Int
    ](ptr, length)


fn _proven_http_url_decode(
    ptr: UnsafePointer[UInt8], length: Int
) -> StringResult:
    """Raw FFI call to proven_http_url_decode."""
    return external_call[
        "proven_http_url_decode", StringResult, UnsafePointer[UInt8], Int
    ](ptr, length)


# =============================================================================
# URL parse FFI (complex struct with multiple heap-allocated fields)
# =============================================================================

# ProvenUrlComponents and ProvenUrlResult contain multiple char* fields that
# require proven_url_free() to release. Since Mojo's external_call works best
# with trivial register-passable types, we provide a raw FFI function that
# returns the full struct and then immediately extract Mojo-owned copies.

# NOTE: Due to the complexity of ProvenUrlComponents (6 string fields + port),
# a full @register_passable struct mapping requires careful layout matching.
# We provide the URL parse functionality below with appropriate C layout.


@register_passable("trivial")
struct _CUrlComponents:
    """C layout mirror of ProvenUrlComponents for FFI.

    Fields:
        scheme, scheme_len, host, host_len, port, has_port,
        path, path_len, query, query_len, fragment, fragment_len.
    """

    var scheme: UnsafePointer[UInt8]
    var scheme_len: Int
    var host: UnsafePointer[UInt8]
    var host_len: Int
    var port: UInt16
    var has_port: Bool
    var path: UnsafePointer[UInt8]
    var path_len: Int
    var query: UnsafePointer[UInt8]
    var query_len: Int
    var fragment: UnsafePointer[UInt8]
    var fragment_len: Int

    fn __init__(out self):
        self.scheme = UnsafePointer[UInt8]()
        self.scheme_len = 0
        self.host = UnsafePointer[UInt8]()
        self.host_len = 0
        self.port = 0
        self.has_port = False
        self.path = UnsafePointer[UInt8]()
        self.path_len = 0
        self.query = UnsafePointer[UInt8]()
        self.query_len = 0
        self.fragment = UnsafePointer[UInt8]()
        self.fragment_len = 0


@register_passable("trivial")
struct _CUrlResult:
    """C layout mirror of ProvenUrlResult: { int32_t status; ProvenUrlComponents }."""

    var status: Int32
    var components: _CUrlComponents

    fn __init__(out self):
        self.status = -1
        self.components = _CUrlComponents()


struct UrlComponents:
    """Mojo-owned URL components. All strings are Mojo-managed (no C pointers)."""

    var scheme: String
    var host: String
    var port: UInt16
    var has_port: Bool
    var path: String
    var query: String
    var fragment: String

    fn __init__(out self):
        self.scheme = ""
        self.host = ""
        self.port = 0
        self.has_port = False
        self.path = ""
        self.query = ""
        self.fragment = ""


fn _copy_c_string(ptr: UnsafePointer[UInt8], length: Int) -> String:
    """Copy a C string (ptr, length) into a Mojo-owned String."""
    if length == 0:
        return String("")
    var buf = List[UInt8](capacity=length + 1)
    for i in range(length):
        buf.append(ptr[i])
    buf.append(0)
    return String(buf^)


fn url_parse(url: String) -> Optional[UrlComponents]:
    """Parse a URL into its components (scheme, host, port, path, query, fragment).

    Returns None if the URL is invalid or cannot be parsed.
    All computation performed in formally verified Idris 2 code.

    The caller owns the returned UrlComponents; no C memory is leaked.
    """
    var pair = _str_to_ptr(url)
    var raw = external_call[
        "proven_url_parse", _CUrlResult, UnsafePointer[UInt8], Int
    ](pair[0], pair[1])

    if raw.status != 0:
        return None

    # Copy all C strings into Mojo-owned Strings
    var result = UrlComponents()
    result.scheme = _copy_c_string(raw.components.scheme, raw.components.scheme_len)
    result.host = _copy_c_string(raw.components.host, raw.components.host_len)
    result.port = raw.components.port
    result.has_port = raw.components.has_port
    result.path = _copy_c_string(raw.components.path, raw.components.path_len)
    result.query = _copy_c_string(raw.components.query, raw.components.query_len)
    result.fragment = _copy_c_string(
        raw.components.fragment, raw.components.fragment_len
    )

    # Free the C-allocated components via proven_url_free.
    # proven_url_free takes a ProvenUrlComponents* -- we pass a pointer to the
    # components sub-struct within our stack-allocated _CUrlResult.
    var components_ptr = UnsafePointer.address_of(raw.components)
    external_call[
        "proven_url_free", NoneType, UnsafePointer[_CUrlComponents]
    ](components_ptr)

    return result


fn url_encode(input: String) -> Optional[String]:
    """Percent-encode a string per RFC 3986.

    Unreserved characters (A-Za-z0-9-._~) pass through; all others become %XX.
    Returns None on error.
    All computation performed in formally verified Idris 2 code.
    """
    var pair = _str_to_ptr(input)
    var result = _proven_http_url_encode(pair[0], pair[1])
    return string_result_to_string(result)


fn url_decode(input: String) -> Optional[String]:
    """Decode a percent-encoded string.

    Returns None on error (e.g. malformed percent sequences).
    All computation performed in formally verified Idris 2 code.
    """
    var pair = _str_to_ptr(input)
    var result = _proven_http_url_decode(pair[0], pair[1])
    return string_result_to_string(result)
