;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; proven-safe-path.el - Safe path operation wrappers for libproven.
;; All computation delegates to the Idris 2 verified core via FFI.
;; Do NOT reimplement any path logic in Emacs Lisp.

;;; Commentary:
;;
;; Provides directory traversal detection and filename sanitization.
;; Every function returns nil on error instead of signaling.

;;; Code:

(require 'proven-ffi)

;;; ============================================================================
;;; Safe path operations
;;; ============================================================================

(defun proven-safe-path-has-traversal-p (path)
  "Return non-nil if PATH contains directory traversal sequences (\"..\").
Useful for preventing path traversal attacks.
Calls proven_path_has_traversal via FFI."
  (condition-case nil
      (proven-ffi--extract-bool-result (proven--ffi-path-has-traversal path))
    (error nil)))

(defun proven-safe-path-sanitize-filename (filename)
  "Sanitize FILENAME by removing dangerous characters.
Returns the sanitized filename string, or nil on error.
Calls proven_path_sanitize_filename via FFI."
  (condition-case nil
      (proven-ffi--extract-string-result (proven--ffi-path-sanitize-filename filename))
    (error nil)))

(provide 'proven-safe-path)

;;; proven-safe-path.el ends here
