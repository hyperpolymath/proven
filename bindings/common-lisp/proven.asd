;;;; SPDX-License-Identifier: PMPL-1.0
;;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;;
;;;; Proven - Safe, validated operations library for Common Lisp
;;;; Version: 0.4.0
;;;; Module Count: 38

(asdf:defsystem #:proven
  :description "Safe, validated operations library for Common Lisp"
  :author "Hyperpolymath"
  :license "PMPL-1.0"
  :version "0.4.0"
  :serial t
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 ;; Core (11 modules)
                 (:file "safe-math")
                 (:file "safe-string")
                 (:file "safe-path")
                 (:file "safe-email")
                 (:file "safe-url")
                 (:file "safe-network")
                 (:file "safe-crypto")
                 (:file "safe-uuid")
                 (:file "safe-currency")
                 (:file "safe-phone")
                 (:file "safe-hex")
                 ;; Data (7 modules)
                 (:file "safe-json")
                 (:file "safe-datetime")
                 (:file "safe-float")
                 (:file "safe-version")
                 (:file "safe-color")
                 (:file "safe-angle")
                 (:file "safe-unit")
                 ;; Data Structures (5 modules)
                 (:file "safe-buffer")
                 (:file "safe-queue")
                 (:file "safe-bloom")
                 (:file "safe-lru")
                 (:file "safe-graph")
                 ;; Resilience (4 modules)
                 (:file "safe-rate-limiter")
                 (:file "safe-circuit-breaker")
                 (:file "safe-retry")
                 (:file "safe-monotonic")
                 ;; State (2 modules)
                 (:file "safe-state-machine")
                 (:file "safe-calculator")
                 ;; Algorithm (4 modules)
                 (:file "safe-geo")
                 (:file "safe-probability")
                 (:file "safe-checksum")
                 (:file "safe-tensor")
                 ;; Security (2 modules)
                 (:file "safe-password")
                 (:file "safe-ml")
                 ;; HTTP (3 modules)
                 (:file "safe-header")
                 (:file "safe-cookie")
                 (:file "safe-content-type")))))
