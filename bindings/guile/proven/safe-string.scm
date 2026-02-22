;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;
;;; SafeString - FFI bindings to libproven string escaping
;;;
;;; All computation delegates to Idris 2 via the Zig FFI layer.
;;; This module is a thin wrapper only -- no reimplemented logic.

(define-module (proven safe-string)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (escape-html
            escape-sql
            escape-js
            is-valid-utf8?))

;;; Load libproven shared library
(define libproven (dynamic-link "libproven"))

;;; FFI function bindings
;;; StringResult is { i32 status, pointer value, size_t length }

(define ffi-escape-html
  (pointer->procedure '* (dynamic-func "proven_string_escape_html" libproven)
                      (list '* size_t)))

(define ffi-escape-sql
  (pointer->procedure '* (dynamic-func "proven_string_escape_sql" libproven)
                      (list '* size_t)))

(define ffi-escape-js
  (pointer->procedure '* (dynamic-func "proven_string_escape_js" libproven)
                      (list '* size_t)))

(define ffi-is-valid-utf8
  (pointer->procedure '* (dynamic-func "proven_string_is_valid_utf8" libproven)
                      (list '* size_t)))

(define ffi-free-string
  (pointer->procedure void (dynamic-func "proven_free_string" libproven)
                      (list '*)))

;;; Helper: convert Scheme string to (pointer, length) pair for FFI
(define (string->ffi-args str)
  (let* ((bv (string->utf8 str))
         (len (bytevector-length bv))
         (ptr (bytevector->pointer bv)))
    (values ptr len)))

;;; Helper: parse StringResult struct { i32 status, ptr value, size_t length }
(define (parse-string-result ptr)
  (let* ((bv (pointer->bytevector ptr (+ 4 (sizeof '*) (sizeof size_t))))
         (status (bytevector-s32-native-ref bv 0))
         (str-ptr-offset (if (= (sizeof '*) 8) 8 4))
         (str-ptr (make-pointer (bytevector-uint-native-ref bv str-ptr-offset (sizeof '*))))
         (len-offset (+ str-ptr-offset (sizeof '*)))
         (len (bytevector-uint-native-ref bv len-offset (sizeof size_t))))
    (if (= status 0)
        (let ((result (pointer->string str-ptr len)))
          (ffi-free-string str-ptr)
          result)
        "")))

;;; Helper: parse BoolResult struct { i32 status, bool value }
(define (parse-bool-result ptr)
  (let* ((bv (pointer->bytevector ptr 8))
         (status (bytevector-s32-native-ref bv 0))
         (value (bytevector-s32-native-ref bv 4)))
    (and (= status 0) (not (= value 0)))))

;;; Escape HTML special characters (delegates to Idris 2)
(define (escape-html input)
  (call-with-values (lambda () (string->ffi-args input))
    (lambda (ptr len)
      (parse-string-result (ffi-escape-html ptr len)))))

;;; Escape SQL single quotes (delegates to Idris 2)
(define (escape-sql input)
  (call-with-values (lambda () (string->ffi-args input))
    (lambda (ptr len)
      (parse-string-result (ffi-escape-sql ptr len)))))

;;; Escape JavaScript special characters (delegates to Idris 2)
(define (escape-js input)
  (call-with-values (lambda () (string->ffi-args input))
    (lambda (ptr len)
      (parse-string-result (ffi-escape-js ptr len)))))

;;; Check if string is valid UTF-8 (delegates to Idris 2)
(define (is-valid-utf8? input)
  (call-with-values (lambda () (string->ffi-args input))
    (lambda (ptr len)
      (parse-bool-result (ffi-is-valid-utf8 ptr len)))))
