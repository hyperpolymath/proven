# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# proven/safe-email.janet - Safe email validation wrappers for libproven.
# All computation delegates to the Idris 2 verified core via Janet FFI.
# Do NOT reimplement any email validation logic in Janet.

(import ./ffi :prefix "ffi/")

## ============================================================================
## Safe email validation
## ============================================================================

(defn valid?
  "Validate email address (RFC 5321 simplified). Returns nil on error."
  [email]
  (ffi/extract-bool-result (ffi/proven-email-is-valid email (length email))))
