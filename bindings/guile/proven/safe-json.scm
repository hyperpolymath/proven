;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;
;;; SafeJson - FFI bindings to libproven JSON validation
;;;
;;; All computation delegates to Idris 2 via the Zig FFI layer.
;;; This module is a thin wrapper only -- no reimplemented logic.

(define-module (proven safe-json)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (json-valid?
            json-type-of))

;;; Load libproven shared library
(define libproven (dynamic-link "libproven"))

;;; FFI function bindings (IdrisValue-based API)

(define ffi-json-is-valid
  (pointer->procedure '* (dynamic-func "proven_json_is_valid" libproven)
                      (list '*)))

(define ffi-json-get-type
  (pointer->procedure '* (dynamic-func "proven_json_get_type" libproven)
                      (list '*)))

;;; Helper: parse IdrisValue as boolean
(define (parse-idris-bool ptr)
  (let* ((bv (pointer->bytevector ptr (sizeof '*)))
         (val (bytevector-uint-native-ref bv 0 (sizeof '*))))
    (not (= val 0))))

;;; Check if JSON string is valid (delegates to Idris 2)
(define (json-valid? json-string)
  (parse-idris-bool
   (ffi-json-is-valid (string->pointer json-string))))

;;; Get type of JSON value (delegates to Idris 2)
;;; Returns a string describing the JSON type
(define (json-type-of json-string)
  (let ((result-ptr (ffi-json-get-type (string->pointer json-string))))
    (pointer->string result-ptr)))
