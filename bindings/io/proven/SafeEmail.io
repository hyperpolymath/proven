# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeEmail.io - Email validation for the Io language.
#
# All operations delegate to libproven via the native C addon (IoProven.c).
# Returns nil on error.
#
# Usage:
#   Proven init
#   result := Proven isValidEmail("user@example.com")
#   result println   # true
#   Proven deinit

SafeEmail := Proven clone do(
    //doc SafeEmail category Safety
    //doc SafeEmail description RFC 5321 email address validation.

    //doc SafeEmail isValidEmail(email) Validate an email address. Returns true/false, nil on error.
    # isValidEmail is provided by the native C addon (IoProven.c).
)
