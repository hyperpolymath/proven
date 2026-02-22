;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

;; Proven - Code that cannot crash.
;;
;; ClojureScript bindings for the libproven formally verified safety
;; library. All computation delegates to Idris 2 + Zig via transitive
;; FFI through the JavaScript binding layer. This code NEVER
;; reimplements logic.
;;
;; Core modules:
;;   proven.safe-math    - Arithmetic without overflow/underflow/div-by-zero
;;   proven.safe-string  - UTF-8 validation and escaping
;;   proven.safe-path    - Filesystem traversal prevention
;;   proven.safe-email   - Email validation (RFC 5321)
;;   proven.safe-url     - URL parsing and validation
;;   proven.safe-crypto  - Constant-time comparison, secure random bytes
;;   proven.safe-json    - JSON validation and type detection

(ns proven.core
  "Main entry point for the proven ClojureScript library."
  (:require [proven.ffi :as ffi]))

(def version "1.0.0")

(def PROVEN-OK                     ffi/PROVEN-OK)
(def PROVEN-ERR-NULL-POINTER       ffi/PROVEN-ERR-NULL-POINTER)
(def PROVEN-ERR-INVALID-ARGUMENT   ffi/PROVEN-ERR-INVALID-ARGUMENT)
(def PROVEN-ERR-OVERFLOW           ffi/PROVEN-ERR-OVERFLOW)
(def PROVEN-ERR-UNDERFLOW          ffi/PROVEN-ERR-UNDERFLOW)
(def PROVEN-ERR-DIVISION-BY-ZERO   ffi/PROVEN-ERR-DIVISION-BY-ZERO)
(def PROVEN-ERR-PARSE-FAILURE      ffi/PROVEN-ERR-PARSE-FAILURE)
(def PROVEN-ERR-VALIDATION-FAILED  ffi/PROVEN-ERR-VALIDATION-FAILED)
(def PROVEN-ERR-OUT-OF-BOUNDS      ffi/PROVEN-ERR-OUT-OF-BOUNDS)
(def PROVEN-ERR-ENCODING-ERROR     ffi/PROVEN-ERR-ENCODING-ERROR)
(def PROVEN-ERR-ALLOCATION-FAILED  ffi/PROVEN-ERR-ALLOCATION-FAILED)
(def PROVEN-ERR-NOT-IMPLEMENTED    ffi/PROVEN-ERR-NOT-IMPLEMENTED)
