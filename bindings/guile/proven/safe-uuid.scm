;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;
;;; SafeUuid - FFI bindings to libproven UUID operations
;;;
;;; All computation delegates to Idris 2 via the Zig FFI layer.
;;; This module is a thin wrapper only -- no reimplemented logic.

(define-module (proven safe-uuid)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (uuid-v4
            uuid-parse
            uuid->string
            uuid-nil?
            uuid-version))

;;; Load libproven shared library
(define libproven (dynamic-link "libproven"))

;;; FFI function bindings
;;; UUID is 16 bytes; UUIDResult is { i32 status, [16]u8 bytes }

(define ffi-uuid-v4
  (pointer->procedure '* (dynamic-func "proven_uuid_v4" libproven) '()))

(define ffi-uuid-parse
  (pointer->procedure '* (dynamic-func "proven_uuid_parse" libproven)
                      (list '* size_t)))

(define ffi-uuid-to-string
  (pointer->procedure '* (dynamic-func "proven_uuid_to_string" libproven)
                      ;; UUID passed as 16 bytes (platform-dependent passing)
                      (list '*)))

(define ffi-uuid-is-nil
  (pointer->procedure int32 (dynamic-func "proven_uuid_is_nil" libproven)
                      (list '*)))

(define ffi-uuid-version
  (pointer->procedure uint8 (dynamic-func "proven_uuid_version" libproven)
                      (list '*)))

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

;;; Generate a random UUID v4 (delegates to Idris 2)
;;; Returns UUID as bytevector or #f on failure
(define (uuid-v4)
  (let* ((result-ptr (ffi-uuid-v4))
         (bv (pointer->bytevector result-ptr 20))  ; status(4) + uuid(16)
         (status (bytevector-s32-native-ref bv 0)))
    (if (= status 0)
        (let ((uuid-bv (make-bytevector 16)))
          (bytevector-copy! bv 4 uuid-bv 0 16)
          uuid-bv)
        #f)))

;;; Parse UUID string into bytevector (delegates to Idris 2)
(define (uuid-parse uuid-string)
  (call-with-values (lambda () (string->ffi-args uuid-string))
    (lambda (ptr len)
      (let* ((result-ptr (ffi-uuid-parse ptr len))
             (bv (pointer->bytevector result-ptr 20))
             (status (bytevector-s32-native-ref bv 0)))
        (if (= status 0)
            (let ((uuid-bv (make-bytevector 16)))
              (bytevector-copy! bv 4 uuid-bv 0 16)
              uuid-bv)
            #f)))))

;;; Convert UUID bytevector to string (delegates to Idris 2)
(define (uuid->string uuid-bv)
  (parse-string-result (ffi-uuid-to-string (bytevector->pointer uuid-bv))))

;;; Check if UUID is nil (all zeros) (delegates to Idris 2)
(define (uuid-nil? uuid-bv)
  (not (= 0 (ffi-uuid-is-nil (bytevector->pointer uuid-bv)))))

;;; Get UUID version (delegates to Idris 2)
(define (uuid-version uuid-bv)
  (ffi-uuid-version (bytevector->pointer uuid-bv)))
