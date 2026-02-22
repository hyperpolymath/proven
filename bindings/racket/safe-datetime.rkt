#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeDatetime - FFI bindings to libproven datetime operations
;;
;; All computation delegates to Idris 2 via the Zig FFI layer.

(require ffi/unsafe)

(provide datetime-parse datetime-format-iso8601
         is-leap-year? days-in-month)

(define libproven (ffi-lib "libproven"))

(define-cstruct _StringResult
  ([status _int32]
   [value  _pointer]
   [length _size]))

(define ffi-datetime-parse
  (get-ffi-obj "proven_datetime_parse" libproven
               (_fun _pointer _size -> _pointer)))

(define ffi-datetime-format-iso8601
  (get-ffi-obj "proven_datetime_format_iso8601" libproven
               (_fun _pointer -> _StringResult)))

(define ffi-datetime-is-leap-year
  (get-ffi-obj "proven_datetime_is_leap_year" libproven
               (_fun _int32 -> _bool)))

(define ffi-datetime-days-in-month
  (get-ffi-obj "proven_datetime_days_in_month" libproven
               (_fun _int32 _uint8 -> _uint8)))

(define ffi-free-string
  (get-ffi-obj "proven_free_string" libproven (_fun _pointer -> _void)))

;; Parse datetime string (delegates to Idris 2)
(define (datetime-parse datetime-string)
  (define bstr (string->bytes/utf-8 datetime-string))
  (ffi-datetime-parse bstr (bytes-length bstr)))

;; Format datetime as ISO 8601 (delegates to Idris 2)
(define (datetime-format-iso8601 dt-ptr)
  (define sr (ffi-datetime-format-iso8601 dt-ptr))
  (if (= (StringResult-status sr) 0)
      (let ([str (cast (StringResult-value sr) _pointer _string/utf-8)])
        (ffi-free-string (StringResult-value sr))
        str)
      #f))

;; Check if year is leap year (delegates to Idris 2)
(define (is-leap-year? year)
  (ffi-datetime-is-leap-year year))

;; Get days in month (delegates to Idris 2)
(define (days-in-month year month)
  (ffi-datetime-days-in-month year month))
