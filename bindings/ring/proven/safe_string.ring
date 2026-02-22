# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe string operations for security-sensitive contexts.
# Thin wrapper over libproven FFI -- all logic lives in Idris 2.
# Returns NULL on error.

load "proven/lib_proven.ring"

# Check if a string contains valid UTF-8 bytes.
# Returns 1 (true) or 0 (false).
func is_valid_utf8 value
    if len(value) = 0 return 1 ok
    lib = proven_lib()
    if lib = NULL return 0 ok
    res = calldll(lib, "proven_string_is_valid_utf8", "pi", value, len(value))
    if isNumber(res)
        return res
    ok
    return 0

# Escape single quotes for SQL strings.
# Returns the escaped string or NULL on error.
# Note: Use parameterized queries when possible.
func escape_sql value
    if len(value) = 0 return "" ok
    lib = proven_lib()
    if lib = NULL return NULL ok
    res = calldll(lib, "proven_string_escape_sql", "pi", value, len(value))
    if res = NULL return NULL ok
    # The result is a StringResult struct; extract the string and free it.
    result = getStringFromPointer(res)
    proven_free_string(res)
    return result

# Escape HTML special characters to prevent XSS attacks.
# Returns the escaped string or NULL on error.
func escape_html value
    if len(value) = 0 return "" ok
    lib = proven_lib()
    if lib = NULL return NULL ok
    res = calldll(lib, "proven_string_escape_html", "pi", value, len(value))
    if res = NULL return NULL ok
    result = getStringFromPointer(res)
    proven_free_string(res)
    return result

# Escape JavaScript special characters.
# Returns the escaped string or NULL on error.
func escape_js value
    if len(value) = 0 return "" ok
    lib = proven_lib()
    if lib = NULL return NULL ok
    res = calldll(lib, "proven_string_escape_js", "pi", value, len(value))
    if res = NULL return NULL ok
    result = getStringFromPointer(res)
    proven_free_string(res)
    return result

# Internal helper: get a Ring string from a C string pointer.
func getStringFromPointer ptr
    if ptr = NULL return NULL ok
    # Use Ring's built-in pointer-to-string conversion
    return ptr2str(ptr)
