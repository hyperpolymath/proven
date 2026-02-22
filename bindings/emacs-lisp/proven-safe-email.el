;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; proven-safe-email.el - Safe email validation wrappers for libproven.
;; All computation delegates to the Idris 2 verified core via FFI.
;; Do NOT reimplement any email validation logic in Emacs Lisp.

;;; Commentary:
;;
;; Provides RFC 5321 email address validation backed by the formally
;; verified Idris 2 implementation.  Returns nil on error.

;;; Code:

(require 'proven-ffi)

;;; ============================================================================
;;; Safe email validation
;;; ============================================================================

(defun proven-safe-email-valid-p (email)
  "Return non-nil if EMAIL is a valid email address (RFC 5321 simplified).
Calls proven_email_is_valid via FFI."
  (condition-case nil
      (proven-ffi--extract-bool-result (proven--ffi-email-is-valid email))
    (error nil)))

(provide 'proven-safe-email)

;;; proven-safe-email.el ends here
