# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe email validation operations.
# Thin wrapper over libproven FFI -- all logic lives in Idris.

import std/options
import lib_proven

proc isValid*(email: string): bool =
  ## Check if an email address is valid.
  ## Validation logic is provided by libproven.
  if email.len == 0:
    return false
  let res = provenEmailIsValid(unsafeAddr email[0], csize_t(email.len))
  if res.status == PROVEN_OK:
    return res.value
  false
