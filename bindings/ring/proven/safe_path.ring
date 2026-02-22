# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe filesystem path operations (traversal prevention).
# Thin wrapper over libproven FFI -- all logic lives in Idris 2.
# Returns NULL on error.

load "proven/lib_proven.ring"

# Check if a path contains directory traversal sequences ("..").
# Returns 1 if traversal detected, 0 otherwise.
func path_has_traversal value
    if len(value) = 0 return 0 ok
    lib = proven_lib()
    if lib = NULL return 0 ok
    res = calldll(lib, "proven_path_has_traversal", "pi", value, len(value))
    if isNumber(res)
        return res
    ok
    return 0

# Sanitize a filename by removing dangerous characters.
# Returns the sanitized filename or NULL on error.
func sanitize_filename value
    if len(value) = 0 return "" ok
    lib = proven_lib()
    if lib = NULL return NULL ok
    res = calldll(lib, "proven_path_sanitize_filename", "pi", value, len(value))
    if res = NULL return NULL ok
    result = getStringFromPointer(res)
    proven_free_string(res)
    return result
