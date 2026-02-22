#lang racket/base

;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; Proven - FFI bindings to libproven (Idris 2 verified safety library)
;;
;; All computation is performed in Idris 2 via the Zig FFI layer.
;; This module re-exports all submodule bindings.

(require "safe-math.rkt"
         "safe-string.rkt"
         "safe-path.rkt"
         "safe-email.rkt"
         "safe-network.rkt"
         "safe-crypto.rkt"
         "safe-uuid.rkt"
         "safe-currency.rkt"
         "safe-phone.rkt"
         "safe-hex.rkt"
         "safe-float.rkt"
         "safe-json.rkt"
         "safe-datetime.rkt"
         "safe-url.rkt"
         "safe-angle.rkt"
         "safe-color.rkt"
         "safe-unit.rkt"
         "safe-version.rkt")

(provide (all-from-out "safe-math.rkt")
         (all-from-out "safe-string.rkt")
         (all-from-out "safe-path.rkt")
         (all-from-out "safe-email.rkt")
         (all-from-out "safe-network.rkt")
         (all-from-out "safe-crypto.rkt")
         (all-from-out "safe-uuid.rkt")
         (all-from-out "safe-currency.rkt")
         (all-from-out "safe-phone.rkt")
         (all-from-out "safe-hex.rkt")
         (all-from-out "safe-float.rkt")
         (all-from-out "safe-json.rkt")
         (all-from-out "safe-datetime.rkt")
         (all-from-out "safe-url.rkt")
         (all-from-out "safe-angle.rkt")
         (all-from-out "safe-color.rkt")
         (all-from-out "safe-unit.rkt")
         (all-from-out "safe-version.rkt"))
