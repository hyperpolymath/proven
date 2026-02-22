;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;
;;; SafeHex - FFI bindings to libproven hex encoding/decoding
;;;
;;; All computation delegates to Idris 2 via the Zig FFI layer.
;;; This module is a thin wrapper only -- no reimplemented logic.

(define-module (proven safe-hex)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (hex-encode
            hex-encode-upper
            hex-decode))

;;; Load libproven shared library
(define libproven (dynamic-link "libproven"))

;;; FFI function bindings

(define ffi-hex-encode
  (pointer->procedure '* (dynamic-func "proven_hex_encode" libproven)
                      (list '* size_t int32)))

(define ffi-hex-decode
  (pointer->procedure '* (dynamic-func "proven_hex_decode" libproven)
                      (list '* size_t)))

(define ffi-free-string
  (pointer->procedure void (dynamic-func "proven_free_string" libproven)
                      (list '*)))

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

;;; Encode bytes to hex string (delegates to Idris 2)
(define (hex-encode bv)
  (let* ((ptr (bytevector->pointer bv))
         (len (bytevector-length bv)))
    (parse-string-result (ffi-hex-encode ptr len 0))))

;;; Encode bytes to uppercase hex string (delegates to Idris 2)
(define (hex-encode-upper bv)
  (let* ((ptr (bytevector->pointer bv))
         (len (bytevector-length bv)))
    (parse-string-result (ffi-hex-encode ptr len 1))))

;;; Decode hex string to bytevector (delegates to Idris 2)
;;; Returns #f on invalid hex input
(define (hex-decode hex-string)
  (let* ((str-bv (string->utf8 hex-string))
         (ptr (bytevector->pointer str-bv))
         (len (bytevector-length str-bv))
         (result-ptr (ffi-hex-decode ptr len))
         ;; HexDecodeResult has similar layout to StringResult
         (bv (pointer->bytevector result-ptr (+ 4 (sizeof '*) (sizeof size_t))))
         (status (bytevector-s32-native-ref bv 0))
         (data-ptr-offset (if (= (sizeof '*) 8) 8 4))
         (data-ptr (make-pointer (bytevector-uint-native-ref bv data-ptr-offset (sizeof '*))))
         (data-len-offset (+ data-ptr-offset (sizeof '*)))
         (data-len (bytevector-uint-native-ref bv data-len-offset (sizeof size_t))))
    (if (= status 0)
        (let ((result (pointer->bytevector data-ptr data-len)))
          (ffi-free-string data-ptr)
          result)
        #f)))
