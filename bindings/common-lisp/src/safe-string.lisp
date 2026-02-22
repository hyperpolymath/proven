;;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;;
;;;; SafeString - Thin CFFI wrapper for libproven string operations.

(in-package #:proven)

(defcfun ("proven_string_is_valid_utf8" %string-is-valid-utf8) (:struct bool-result) (ptr :pointer) (len :size))
(defcfun ("proven_string_escape_sql" %string-escape-sql) (:struct string-result) (ptr :pointer) (len :size))
(defcfun ("proven_string_escape_html" %string-escape-html) (:struct string-result) (ptr :pointer) (len :size))
(defcfun ("proven_string_escape_js" %string-escape-js) (:struct string-result) (ptr :pointer) (len :size))

(defun string-is-valid-utf8 (str)
  "Check if STR is valid UTF-8. Returns (values bool ok-p)."
  (with-foreign-string-buf (ptr len str)
    (extract-bool-result (%string-is-valid-utf8 ptr len))))

(defun string-escape-sql (str)
  "Escape STR for SQL. Returns (values escaped-string t) or (values nil nil)."
  (with-foreign-string-buf (ptr len str)
    (extract-string-result (%string-escape-sql ptr len))))

(defun string-escape-html (str)
  "Escape STR for HTML. Returns (values escaped-string t) or (values nil nil)."
  (with-foreign-string-buf (ptr len str)
    (extract-string-result (%string-escape-html ptr len))))

(defun string-escape-js (str)
  "Escape STR for JavaScript. Returns (values escaped-string t) or (values nil nil)."
  (with-foreign-string-buf (ptr len str)
    (extract-string-result (%string-escape-js ptr len))))
