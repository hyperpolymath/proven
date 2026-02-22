;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; proven-safe-string.el - Safe string operation wrappers for libproven.
;; All computation delegates to the Idris 2 verified core via FFI.
;; Do NOT reimplement any string logic in Emacs Lisp.

;;; Commentary:
;;
;; Provides UTF-8 validation and context-aware string escaping (SQL, HTML, JS).
;; Every function returns nil on error instead of signaling.

;;; Code:

(require 'proven-ffi)

;;; ============================================================================
;;; Safe string operations
;;; ============================================================================

(defun proven-safe-string-valid-utf8-p (str)
  "Return non-nil if STR contains valid UTF-8 bytes.
Calls proven_string_is_valid_utf8 via FFI."
  (condition-case nil
      (proven-ffi--extract-bool-result (proven--ffi-string-is-valid-utf8 str))
    (error nil)))

(defun proven-safe-string-escape-sql (str)
  "Escape STR for safe use in SQL queries (single-quote escaping).
Returns the escaped string, or nil on error.
NOTE: Prefer parameterized queries over string escaping.
Calls proven_string_escape_sql via FFI."
  (condition-case nil
      (proven-ffi--extract-string-result (proven--ffi-string-escape-sql str))
    (error nil)))

(defun proven-safe-string-escape-html (str)
  "Escape STR for safe embedding in HTML (prevents XSS).
Returns the escaped string, or nil on error.
Calls proven_string_escape_html via FFI."
  (condition-case nil
      (proven-ffi--extract-string-result (proven--ffi-string-escape-html str))
    (error nil)))

(defun proven-safe-string-escape-js (str)
  "Escape STR for safe embedding in JavaScript string literals.
Returns the escaped string, or nil on error.
Calls proven_string_escape_js via FFI."
  (condition-case nil
      (proven-ffi--extract-string-result (proven--ffi-string-escape-js str))
    (error nil)))

(provide 'proven-safe-string)

;;; proven-safe-string.el ends here
