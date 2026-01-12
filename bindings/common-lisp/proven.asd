;;;; SPDX-License-Identifier: PMPL-1.0
;;;; SPDX-FileCopyrightText: 2025 Hyperpolymath
;;;;
;;;; Proven - Safe, validated operations library for Common Lisp

(asdf:defsystem #:proven
  :description "Safe, validated operations library for Common Lisp"
  :author "Hyperpolymath"
  :license "PMPL-1.0"
  :version "0.1.0"
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "safe-math")
                             (:file "safe-string")
                             (:file "safe-path")
                             (:file "safe-email")
                             (:file "safe-network")
                             (:file "safe-crypto")
                             (:file "safe-uuid")
                             (:file "safe-currency")
                             (:file "safe-phone")
                             (:file "safe-hex")))))
