;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;
;;; SafePhone - FFI bindings to libproven phone number operations
;;;
;;; All computation delegates to Idris 2 via the Zig FFI layer.
;;; This module is a thin wrapper only -- no reimplemented logic.

(define-module (proven safe-phone)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (phone-parse
            phone-format-e164))

;;; Load libproven shared library
(define libproven (dynamic-link "libproven"))

;;; FFI function bindings

(define ffi-phone-parse
  (pointer->procedure '* (dynamic-func "proven_phone_parse" libproven)
                      (list '* size_t)))

(define ffi-phone-format-e164
  (pointer->procedure '* (dynamic-func "proven_phone_format_e164" libproven)
                      (list uint16 uint64)))

(define ffi-free-string
  (pointer->procedure void (dynamic-func "proven_free_string" libproven)
                      (list '*)))

;;; Helper: convert string to (pointer, length)
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
        #f)))

;;; Parse phone number string (delegates to Idris 2)
;;; Returns PhoneResult or #f on failure
(define (phone-parse phone-string)
  (call-with-values (lambda () (string->ffi-args phone-string))
    (lambda (ptr len)
      (ffi-phone-parse ptr len))))

;;; Format phone number as E.164 (delegates to Idris 2)
;;; country-code: numeric country code (e.g., 1 for US)
;;; national-number: the national portion of the number
(define (phone-format-e164 country-code national-number)
  (parse-string-result
   (ffi-phone-format-e164 country-code national-number)))
