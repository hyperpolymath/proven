;;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;;
;;;; SafeEmail - Thin CFFI wrapper for libproven email validation.

(in-package #:proven)

(defcfun ("proven_email_is_valid" %email-is-valid) (:struct bool-result) (ptr :pointer) (len :size))

(defun email-is-valid (email)
  "Validate EMAIL address. Returns (values bool ok-p)."
  (with-foreign-string-buf (ptr len email)
    (extract-bool-result (%email-is-valid ptr len))))
