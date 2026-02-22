;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;
;;; SafeDatetime - FFI bindings to libproven datetime operations
;;;
;;; All computation delegates to Idris 2 via the Zig FFI layer.
;;; This module is a thin wrapper only -- no reimplemented logic.

(define-module (proven safe-datetime)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (datetime-parse
            datetime-format-iso8601
            leap-year?
            days-in-month))

;;; Load libproven shared library
(define libproven (dynamic-link "libproven"))

;;; FFI function bindings

(define ffi-datetime-parse
  (pointer->procedure '* (dynamic-func "proven_datetime_parse" libproven)
                      (list '* size_t)))

(define ffi-datetime-format-iso8601
  (pointer->procedure '* (dynamic-func "proven_datetime_format_iso8601" libproven)
                      (list '*)))

(define ffi-datetime-is-leap-year
  (pointer->procedure int32 (dynamic-func "proven_datetime_is_leap_year" libproven)
                      (list int32)))

(define ffi-datetime-days-in-month
  (pointer->procedure uint8 (dynamic-func "proven_datetime_days_in_month" libproven)
                      (list int32 uint8)))

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

;;; Parse datetime string (delegates to Idris 2)
;;; Returns DateTimeResult or #f on failure
(define (datetime-parse datetime-string)
  (call-with-values (lambda () (string->ffi-args datetime-string))
    (lambda (ptr len)
      (ffi-datetime-parse ptr len))))

;;; Format datetime as ISO 8601 string (delegates to Idris 2)
;;; dt-ptr: pointer to DateTime struct from datetime-parse
(define (datetime-format-iso8601 dt-ptr)
  (parse-string-result (ffi-datetime-format-iso8601 dt-ptr)))

;;; Check if year is a leap year (delegates to Idris 2)
(define (leap-year? year)
  (not (= 0 (ffi-datetime-is-leap-year year))))

;;; Get number of days in a month (delegates to Idris 2)
(define (days-in-month year month)
  (ffi-datetime-days-in-month year month))
