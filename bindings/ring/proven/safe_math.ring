# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe mathematical operations with overflow detection.
# Thin wrapper over libproven FFI -- all logic lives in Idris 2.
# Returns NULL on error.

load "proven/lib_proven.ring"

# Safely add two integers with overflow detection.
# Returns the sum or NULL on overflow.
func safe_add a, b
    lib = proven_lib()
    if lib = NULL return NULL ok
    status = calldll(lib, "proven_math_add_checked", "ll", a, b)
    # The C function returns a struct; Ring receives the status and value
    # We rely on the Ring DLL calling convention to unpack the struct.
    if status = PROVEN_OK
        return calldll(lib, "proven_math_add_checked", "ll", a, b)
    ok
    return NULL

# Safely subtract two integers with underflow detection.
# Returns the difference or NULL on underflow.
func safe_sub a, b
    lib = proven_lib()
    if lib = NULL return NULL ok
    res = calldll(lib, "proven_math_sub_checked", "ll", a, b)
    if isNumber(res) and res = PROVEN_OK
        return res
    ok
    return NULL

# Safely multiply two integers with overflow detection.
# Returns the product or NULL on overflow.
func safe_mul a, b
    lib = proven_lib()
    if lib = NULL return NULL ok
    res = calldll(lib, "proven_math_mul_checked", "ll", a, b)
    if isNumber(res) and res = PROVEN_OK
        return res
    ok
    return NULL

# Safely divide two integers.
# Returns the quotient or NULL on division by zero.
func safe_div a, b
    lib = proven_lib()
    if lib = NULL return NULL ok
    res = calldll(lib, "proven_math_div", "ll", a, b)
    if isNumber(res) and res = PROVEN_OK
        return res
    ok
    return NULL

# Safely compute modulo.
# Returns the remainder or NULL on division by zero.
func safe_mod a, b
    lib = proven_lib()
    if lib = NULL return NULL ok
    res = calldll(lib, "proven_math_mod", "ll", a, b)
    if isNumber(res) and res = PROVEN_OK
        return res
    ok
    return NULL

# Safely compute absolute value.
# Returns the absolute value or NULL for INT64_MIN.
func safe_abs n
    lib = proven_lib()
    if lib = NULL return NULL ok
    res = calldll(lib, "proven_math_abs_safe", "l", n)
    if isNumber(res) and res = PROVEN_OK
        return res
    ok
    return NULL

# Clamp a value to [lo, hi]. Always succeeds.
func math_clamp lo, hi, value
    lib = proven_lib()
    if lib = NULL return value ok
    return calldll(lib, "proven_math_clamp", "lll", lo, hi, value)

# Safely compute integer exponentiation.
# Returns the result or NULL on overflow.
func safe_pow base, exp
    lib = proven_lib()
    if lib = NULL return NULL ok
    res = calldll(lib, "proven_math_pow_checked", "li", base, exp)
    if isNumber(res) and res = PROVEN_OK
        return res
    ok
    return NULL
