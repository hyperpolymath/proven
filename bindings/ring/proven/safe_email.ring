# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe email validation (RFC 5321 simplified).
# Thin wrapper over libproven FFI -- all logic lives in Idris 2.

load "proven/lib_proven.ring"

# Validate an email address per RFC 5321.
# Returns 1 if valid, 0 otherwise.
func is_valid_email value
    if len(value) = 0 return 0 ok
    lib = proven_lib()
    if lib = NULL return 0 ok
    res = calldll(lib, "proven_email_is_valid", "pi", value, len(value))
    if isNumber(res)
        return res
    ok
    return 0
