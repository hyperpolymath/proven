;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

;; SafeEmail - Email address validation (RFC 5321 simplified).
;;
;; All functions delegate to libproven via JS interop (transitive FFI).
;; Returns nil on error. NEVER reimplements logic.

(ns proven.safe-email
  "Safe email validation per RFC 5321.
   All computation delegates to the Idris 2 core via FFI."
  (:require [proven.ffi :as ffi]))


(defn valid-email?
  "Validate an email address per RFC 5321 (simplified).
  Returns true if valid, false if invalid, nil on FFI error.

  Example:
    (valid-email? \"user@example.com\")  ;=> true
    (valid-email? \"not-an-email\")      ;=> false"
  [email]
  (when-let [lib (ffi/get-lib)]
    (let [result (.proven_email_is_valid lib email (count email))]
      (when (ffi/ok? (.-status result))
        (.-value result)))))
