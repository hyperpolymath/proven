# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

# cython: language_level=3

"""
SafeUrl - URL parsing and validation.

All functions delegate to libproven via direct C FFI calls.
Returns None on error. NEVER reimplements logic.
"""

from libc.stdint cimport uint8_t
from libc.stddef cimport size_t

cimport proven.lib_proven as c


def parse_url(str url_str):
    """Parse a URL string into a dictionary of components.
    Returns a dict with keys: scheme, host, port, has_port, path, query, fragment.
    Returns None on parse failure.

    >>> result = parse_url("https://example.com:8080/path?q=1#frag")
    >>> result["scheme"]
    'https'
    >>> result["host"]
    'example.com'
    >>> result["port"]
    8080
    """
    cdef bytes b = url_str.encode("utf-8")
    cdef const uint8_t* ptr = <const uint8_t*><char*>b
    cdef size_t length = len(b)
    cdef c.ProvenUrlResult result
    with nogil:
        result = c.proven_url_parse(ptr, length)
    if result.status != c.PROVEN_OK:
        return None

    cdef c.ProvenUrlComponents comp = result.components
    parsed = {
        "scheme":   comp.scheme[:comp.scheme_len].decode("utf-8", errors="replace") if comp.scheme != NULL else "",
        "host":     comp.host[:comp.host_len].decode("utf-8", errors="replace") if comp.host != NULL else "",
        "port":     int(comp.port),
        "has_port": bool(comp.has_port),
        "path":     comp.path[:comp.path_len].decode("utf-8", errors="replace") if comp.path != NULL else "",
        "query":    comp.query[:comp.query_len].decode("utf-8", errors="replace") if comp.query != NULL else "",
        "fragment": comp.fragment[:comp.fragment_len].decode("utf-8", errors="replace") if comp.fragment != NULL else "",
    }

    # Free the URL components
    c.proven_url_free(&result.components)
    return parsed
