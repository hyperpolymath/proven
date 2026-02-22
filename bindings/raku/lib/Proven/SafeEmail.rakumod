# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Proven::SafeEmail - Email address validation.
#
# Thin wrapper over libproven's SafeEmail module.  The validation logic
# (RFC 5321 simplified) runs in formally verified Idris 2 code.  This module
# does NOT reimplement any email validation logic.

unit module Proven::SafeEmail;

use NativeCall;
use Proven::LibProven;

# ============================================================================
# Email validation
# ============================================================================

#| Validate an email address (RFC 5321 simplified).
#| Returns True if valid, False if invalid, Nil on internal error.
sub email-is-valid(Str:D $email --> Bool) is export {
    my ($buf, $len) = str-to-buf($email);
    my BoolResult $r = proven_email_is_valid(nativecast(Pointer, $buf), $len);
    return Nil unless $r.status == 0;
    return $r.value;
}
