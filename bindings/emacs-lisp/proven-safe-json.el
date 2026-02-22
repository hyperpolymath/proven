;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; proven-safe-json.el - Safe JSON validation wrappers for libproven.
;; All computation delegates to the Idris 2 verified core via FFI.
;; Do NOT reimplement any JSON logic in Emacs Lisp.

;;; Commentary:
;;
;; Provides JSON validation and type detection backed by the formally
;; verified Idris 2 implementation.  Returns nil on error.

;;; Code:

(require 'proven-ffi)

;;; ============================================================================
;;; JSON type constants
;;; ============================================================================

(defconst proven-json-type-null 0
  "JSON null type.")

(defconst proven-json-type-bool 1
  "JSON boolean type.")

(defconst proven-json-type-number 2
  "JSON number type.")

(defconst proven-json-type-string 3
  "JSON string type.")

(defconst proven-json-type-array 4
  "JSON array type.")

(defconst proven-json-type-object 5
  "JSON object type.")

(defconst proven-json-type-invalid -1
  "Invalid JSON type.")

;;; ============================================================================
;;; Safe JSON operations
;;; ============================================================================

(defun proven-safe-json-valid-p (json-string)
  "Return non-nil if JSON-STRING is valid JSON.
Calls proven_json_is_valid via FFI."
  (condition-case nil
      (proven-ffi--extract-bool-result (proven--ffi-json-is-valid json-string))
    (error nil)))

(defun proven-safe-json-get-type (json-string)
  "Return the root-level JSON type of JSON-STRING as a symbol.
Possible return values: `null', `boolean', `number', `string',
`array', `object', `invalid', or nil on error.
Calls proven_json_get_type via FFI."
  (condition-case nil
      (let ((type-code (proven--ffi-json-get-type json-string)))
        (pcase type-code
          (0  'null)
          (1  'boolean)
          (2  'number)
          (3  'string)
          (4  'array)
          (5  'object)
          (-1 'invalid)
          (_  'invalid)))
    (error nil)))

(provide 'proven-safe-json)

;;; proven-safe-json.el ends here
