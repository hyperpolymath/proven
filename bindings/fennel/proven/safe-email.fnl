;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; proven/safe-email.fnl - Safe email validation wrappers for libproven.
;; All computation delegates to the Idris 2 verified core via LuaJIT FFI.
;; Do NOT reimplement any email validation logic in Fennel.

(local {: lib : bool-result} (require :proven.ffi))

;; ============================================================================
;; Safe email validation
;; ============================================================================

(fn is-valid [s]
  "Validate email address (RFC 5321 simplified). Returns nil on error."
  (bool-result (lib.proven_email_is_valid s (length s))))

;; ============================================================================
;; Export
;; ============================================================================

{:is_valid is-valid}
