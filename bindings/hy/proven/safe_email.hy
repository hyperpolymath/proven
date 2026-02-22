; SPDX-License-Identifier: PMPL-1.0-or-later
; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

;; SafeEmail - Email address validation (RFC 5321 simplified).
;;
;; All functions delegate to libproven via FFI.
;; Returns None on error. NEVER reimplements logic.

(import proven.ffi [get-lib ok? encode-str])


(defn is-valid-email [email]
  "Validate an email address per RFC 5321 (simplified).
  Returns True if valid, False if invalid, None on FFI error.

  Example:
    (is-valid-email \"user@example.com\")  ; => True
    (is-valid-email \"not-an-email\")      ; => False
  "
  (setv #(b n) (encode-str email))
  (setv result (.proven_email_is_valid (get-lib) b n))
  (when (ok? result.status)
    (return result.value))
  None)
