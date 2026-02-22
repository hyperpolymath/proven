;;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;;
;;;; SafePath - Thin CFFI wrapper for libproven path operations.

(in-package #:proven)

(defcfun ("proven_path_has_traversal" %path-has-traversal) (:struct bool-result) (ptr :pointer) (len :size))
(defcfun ("proven_path_sanitize_filename" %path-sanitize-filename) (:struct string-result) (ptr :pointer) (len :size))

(defun path-has-traversal (path)
  "Check if PATH contains directory traversal patterns. Returns (values bool ok-p)."
  (with-foreign-string-buf (ptr len path)
    (extract-bool-result (%path-has-traversal ptr len))))

(defun path-sanitize-filename (filename)
  "Sanitize FILENAME by removing dangerous characters. Returns (values sanitized t) or (values nil nil)."
  (with-foreign-string-buf (ptr len filename)
    (extract-string-result (%path-sanitize-filename ptr len))))
