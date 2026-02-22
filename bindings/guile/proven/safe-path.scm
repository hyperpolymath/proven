;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;
;;; SafePath - FFI bindings to libproven path validation
;;;
;;; All computation delegates to Idris 2 via the Zig FFI layer.
;;; This module is a thin wrapper only -- no reimplemented logic.

(define-module (proven safe-path)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (path-has-traversal?
            sanitize-filename))

;;; Load libproven shared library
(define libproven (dynamic-link "libproven"))

;;; FFI function bindings

(define ffi-path-has-traversal
  (pointer->procedure '* (dynamic-func "proven_path_has_traversal" libproven)
                      (list '*)))

(define ffi-path-sanitize-filename
  (pointer->procedure '* (dynamic-func "proven_path_sanitize_filename" libproven)
                      (list '*)))

(define ffi-free-string
  (pointer->procedure void (dynamic-func "proven_free_string" libproven)
                      (list '*)))

;;; Helper: parse BoolResult from IdrisValue return
(define (parse-idris-bool-result ptr)
  ;; IdrisValue-based returns; the Zig FFI wraps Idris booleans
  ;; For path_has_traversal the return is an IdrisValue encoding bool
  ;; We read it as a native int from the pointer
  (let* ((bv (pointer->bytevector ptr (sizeof '*)))
         (val (bytevector-uint-native-ref bv 0 (sizeof '*))))
    (not (= val 0))))

;;; Helper: parse IdrisValue string result
(define (parse-idris-string-result ptr)
  (let ((str (pointer->string ptr)))
    str))

;;; Check if path contains directory traversal sequences (delegates to Idris 2)
(define (path-has-traversal? path)
  (parse-idris-bool-result
   (ffi-path-has-traversal (string->pointer path))))

;;; Sanitize filename by removing dangerous characters (delegates to Idris 2)
(define (sanitize-filename filename)
  (let* ((result-ptr (ffi-path-sanitize-filename (string->pointer filename)))
         (result (pointer->string result-ptr)))
    result))
