;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2025 Hyperpolymath

(defproject com.hyperpolymath/proven "0.4.0"
  :description "A comprehensive safety library for Clojure providing 38 modules for overflow-checked arithmetic, XSS prevention, path traversal protection, email validation, IP classification, cryptographic operations, data structures, resilience patterns, and more."
  :url "https://github.com/hyperpolymath/proven"
  :license {:name "PMPL-1.0"
            :url "https://github.com/hyperpolymath/proven/blob/main/LICENSE"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/spec.alpha "0.4.233"]]
  :repl-options {:init-ns proven.core}
  :source-paths ["src"]
  :profiles {:dev {:dependencies [[org.clojure/test.check "1.1.1"]]}}

  ;; Module count: 38
  ;; Core (11): safe-math, safe-string, safe-path, safe-email, safe-url, safe-network,
  ;;            safe-crypto, safe-uuid, safe-currency, safe-phone, safe-hex
  ;; Data (7): safe-json, safe-datetime, safe-float, safe-version, safe-color, safe-angle, safe-unit
  ;; Data Structures (5): safe-buffer, safe-queue, safe-bloom, safe-lru, safe-graph
  ;; Resilience (4): safe-rate-limiter, safe-circuit-breaker, safe-retry, safe-monotonic
  ;; State (2): safe-state-machine, safe-calculator
  ;; Algorithm (4): safe-geo, safe-probability, safe-checksum, safe-tensor
  ;; Security (2): safe-password, safe-ml
  ;; HTTP (3): safe-header, safe-cookie, safe-content-type
  )
