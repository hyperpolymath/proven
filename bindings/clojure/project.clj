;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

(defproject com.hyperpolymath/proven "1.0.0"
  :description "Clojure bindings for Proven - formally verified safety primitives via JNA FFI to libproven (Idris 2 + Zig)."
  :url "https://github.com/hyperpolymath/proven"
  :license {:name "PMPL-1.0-or-later"
            :url "https://github.com/hyperpolymath/proven/blob/main/LICENSE"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [net.java.dev.jna/jna "5.14.0"]]
  :repl-options {:init-ns proven.core}
  :source-paths ["src"]
  :profiles {:dev {:dependencies [[org.clojure/test.check "1.1.1"]]}})
