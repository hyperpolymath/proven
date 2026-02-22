;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;
;;; SafeCurrency - FFI bindings to libproven currency operations
;;;
;;; All computation delegates to Idris 2 via the Zig FFI layer.
;;; This module is a thin wrapper only -- no reimplemented logic.

(define-module (proven safe-currency)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (currency-parse
            currency-format))

;;; Load libproven shared library
(define libproven (dynamic-link "libproven"))

;;; FFI function bindings

(define ffi-currency-parse
  (pointer->procedure '* (dynamic-func "proven_currency_parse" libproven)
                      (list '* size_t)))

(define ffi-currency-format
  (pointer->procedure '* (dynamic-func "proven_currency_format" libproven)
                      (list int64 '* uint8)))

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

;;; Parse currency string (delegates to Idris 2)
;;; Returns CurrencyResult or #f on failure
(define (currency-parse currency-string)
  (call-with-values (lambda () (string->ffi-args currency-string))
    (lambda (ptr len)
      (ffi-currency-parse ptr len))))

;;; Format currency amount (delegates to Idris 2)
;;; amount-minor: amount in smallest units (e.g., cents)
;;; currency-code: 3-letter ISO 4217 code (e.g., "USD")
;;; decimal-places: number of decimal places for the currency
(define (currency-format amount-minor currency-code decimal-places)
  (let* ((code-bv (make-bytevector 3 0)))
    ;; Copy up to 3 bytes of the currency code
    (let ((code-str (string->utf8 currency-code)))
      (bytevector-copy! code-str 0 code-bv 0
                        (min 3 (bytevector-length code-str))))
    (parse-string-result
     (ffi-currency-format amount-minor
                          (bytevector->pointer code-bv)
                          decimal-places))))
