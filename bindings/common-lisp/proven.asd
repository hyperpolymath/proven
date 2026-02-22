;;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;;;
;;;; Proven - Thin CFFI wrapper around libproven (formally verified safety library)
;;;; Version: 1.0.0
;;;; All computation delegates to Idris 2 via the Zig FFI layer.

(asdf:defsystem #:proven
  :description "Thin CFFI wrapper around libproven (formally verified safety library)"
  :author "Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>"
  :license "PMPL-1.0-or-later"
  :version "1.0.0"
  :depends-on (#:cffi)
  :serial t
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "ffi")
                 (:file "safe-math")
                 (:file "safe-string")
                 (:file "safe-path")
                 (:file "safe-email")
                 (:file "safe-network")
                 (:file "safe-crypto")
                 (:file "safe-uuid")
                 (:file "safe-currency")
                 (:file "safe-phone")
                 (:file "safe-hex")
                 (:file "safe-url")
                 (:file "safe-json")
                 (:file "safe-datetime")
                 (:file "safe-float")
                 (:file "safe-version")
                 (:file "safe-color")
                 (:file "safe-angle")
                 (:file "safe-unit")
                 (:file "safe-buffer")
                 (:file "safe-queue")
                 (:file "safe-bloom")
                 (:file "safe-lru")
                 (:file "safe-graph")
                 (:file "safe-rate-limiter")
                 (:file "safe-circuit-breaker")
                 (:file "safe-retry")
                 (:file "safe-monotonic")
                 (:file "safe-state-machine")
                 (:file "safe-calculator")
                 (:file "safe-geo")
                 (:file "safe-probability")
                 (:file "safe-checksum")
                 (:file "safe-tensor")
                 (:file "safe-password")
                 (:file "safe-ml")
                 (:file "safe-header")
                 (:file "safe-cookie")
                 (:file "safe-content-type")
                 (:file "safe-http")
                 (:file "safe-registry")
                 (:file "safe-digest")))))
