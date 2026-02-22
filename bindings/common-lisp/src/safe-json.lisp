;;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;;
;;;; SafeJSON - Thin CFFI wrapper for libproven JSON operations.
;;;; All computation delegates to Idris 2 via the Zig FFI layer.

(in-package #:proven)

;;; ============================================================================
;;; JSON type enum matching the Zig FFI ABI
;;; ============================================================================

;;; JsonType enum values from Zig:
;;;   null_   = 0
;;;   bool_   = 1
;;;   number  = 2
;;;   string  = 3
;;;   array   = 4
;;;   object  = 5
;;;   invalid = -1

(defconstant +json-type-null+    0)
(defconstant +json-type-bool+    1)
(defconstant +json-type-number+  2)
(defconstant +json-type-string+  3)
(defconstant +json-type-array+   4)
(defconstant +json-type-object+  5)
(defconstant +json-type-invalid+ -1)

;;; ============================================================================
;;; FFI declarations
;;; ============================================================================

(defcfun ("proven_json_is_valid" %json-is-valid) (:struct bool-result)
  (ptr :pointer) (len :size))

(defcfun ("proven_json_get_type" %json-get-type) :int32
  (ptr :pointer) (len :size))

;;; ============================================================================
;;; Public API
;;; ============================================================================

(defun json-is-valid (str)
  "Check if STR is valid JSON via libproven.
   Returns (values bool ok-p) where bool is T if valid JSON."
  (with-foreign-string-buf (ptr len str)
    (extract-bool-result (%json-is-valid ptr len))))

(defun json-get-type (str)
  "Get the JSON value type of STR at root level via libproven.
   Returns a keyword: :NULL, :BOOLEAN, :NUMBER, :STRING, :ARRAY, :OBJECT,
   or :INVALID if the string cannot be parsed."
  (with-foreign-string-buf (ptr len str)
    (let ((type-code (%json-get-type ptr len)))
      (case type-code
        (#.+json-type-null+    :null)
        (#.+json-type-bool+    :boolean)
        (#.+json-type-number+  :number)
        (#.+json-type-string+  :string)
        (#.+json-type-array+   :array)
        (#.+json-type-object+  :object)
        (otherwise             :invalid)))))
