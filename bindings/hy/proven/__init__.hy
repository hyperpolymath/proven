; SPDX-License-Identifier: PMPL-1.0-or-later
; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

;; Proven - Code that cannot crash.
;;
;; Hy (Lisp-on-Python) bindings for the libproven formally verified
;; safety library. All computation delegates to the Idris 2 + Zig
;; FFI layer via ctypes. This binding NEVER reimplements logic.
;;
;; Core modules:
;;   safe-math     - Arithmetic without overflow/underflow/division-by-zero
;;   safe-string   - UTF-8 validation and escaping
;;   safe-path     - Filesystem traversal prevention
;;   safe-email    - Email validation (RFC 5321)
;;   safe-url      - URL parsing and validation
;;   safe-crypto   - Constant-time comparison, secure random bytes
;;   safe-json     - JSON validation and type detection
;;
;; Usage:
;;   (import proven.safe-math [safe-add safe-div])
;;   (print (safe-add 5 3))   ; => 8
;;   (print (safe-div 10 0))  ; => None

(import proven.ffi [PROVEN-OK
                     PROVEN-ERR-NULL-POINTER
                     PROVEN-ERR-INVALID-ARGUMENT
                     PROVEN-ERR-OVERFLOW
                     PROVEN-ERR-UNDERFLOW
                     PROVEN-ERR-DIVISION-BY-ZERO
                     PROVEN-ERR-PARSE-FAILURE
                     PROVEN-ERR-VALIDATION-FAILED
                     PROVEN-ERR-OUT-OF-BOUNDS
                     PROVEN-ERR-ENCODING-ERROR
                     PROVEN-ERR-ALLOCATION-FAILED
                     PROVEN-ERR-NOT-IMPLEMENTED
                     get-lib
                     ok?])

(setv __version__ "1.0.0")
(setv __all__ ["PROVEN-OK"
               "PROVEN-ERR-NULL-POINTER"
               "PROVEN-ERR-INVALID-ARGUMENT"
               "PROVEN-ERR-OVERFLOW"
               "PROVEN-ERR-UNDERFLOW"
               "PROVEN-ERR-DIVISION-BY-ZERO"
               "PROVEN-ERR-PARSE-FAILURE"
               "PROVEN-ERR-VALIDATION-FAILED"
               "PROVEN-ERR-OUT-OF-BOUNDS"
               "PROVEN-ERR-ENCODING-ERROR"
               "PROVEN-ERR-ALLOCATION-FAILED"
               "PROVEN-ERR-NOT-IMPLEMENTED"
               "get-lib"
               "ok?"])
