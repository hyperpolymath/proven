;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Hyperpolymath

(defproject com.hyperpolymath/proven "0.8.0"
  :description "A safety library for Clojure providing overflow-checked arithmetic, XSS prevention, path traversal protection, email validation, IP classification, and cryptographic operations."
  :url "https://github.com/hyperpolymath/proven"
  :license {:name "AGPL-3.0-or-later"
            :url "https://github.com/hyperpolymath/proven/blob/main/LICENSE"}
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :repl-options {:init-ns proven.core}
  :source-paths ["src"]
  :profiles {:dev {:dependencies [[org.clojure/test.check "1.1.1"]]}})
