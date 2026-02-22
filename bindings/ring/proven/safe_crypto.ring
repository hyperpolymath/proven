# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe cryptographic primitives.
# Thin wrapper over libproven FFI -- all logic lives in Idris 2.
# Returns NULL on error.

load "proven/lib_proven.ring"

# Constant-time byte comparison (timing-attack safe).
# Returns 1 if equal, 0 otherwise.
func constant_time_eq a, b
    lib = proven_lib()
    if lib = NULL return 0 ok
    res = calldll(lib, "proven_crypto_constant_time_eq",
                  "pipi", a, len(a), b, len(b))
    if isNumber(res)
        return res
    ok
    return 0

# Generate cryptographically secure random bytes.
# Returns a string of random bytes or NULL on failure.
func random_bytes count
    lib = proven_lib()
    if lib = NULL return NULL ok
    buf = space(count)
    status = calldll(lib, "proven_crypto_random_bytes", "pi", buf, count)
    if status = PROVEN_OK
        return buf
    ok
    return NULL

# Encode bytes to lowercase hex string. Returns NULL on error.
func hex_encode data
    if len(data) = 0 return "" ok
    lib = proven_lib()
    if lib = NULL return NULL ok
    res = calldll(lib, "proven_hex_encode", "pii", data, len(data), 0)
    if res = NULL return NULL ok
    result = getStringFromPointer(res)
    proven_free_string(res)
    return result

# Encode bytes to uppercase hex string. Returns NULL on error.
func hex_encode_upper data
    if len(data) = 0 return "" ok
    lib = proven_lib()
    if lib = NULL return NULL ok
    res = calldll(lib, "proven_hex_encode", "pii", data, len(data), 1)
    if res = NULL return NULL ok
    result = getStringFromPointer(res)
    proven_free_string(res)
    return result

# Decode hex string to bytes. Returns NULL on error.
func hex_decode hex_str
    if len(hex_str) = 0 return "" ok
    lib = proven_lib()
    if lib = NULL return NULL ok
    res = calldll(lib, "proven_hex_decode", "pi", hex_str, len(hex_str))
    if res = NULL return NULL ok
    # HexDecodeResult has {status, data, length}; extract and free
    result = getStringFromPointer(res)
    calldll(lib, "proven_hex_free", "p", res)
    return result
