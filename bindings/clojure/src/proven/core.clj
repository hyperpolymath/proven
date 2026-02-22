;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; Proven - Clojure bindings to libproven via JNA.
;;
;; Architecture: All computation delegates to Idris 2 verified code through
;; the Zig C ABI. No logic is reimplemented in Clojure. This module is a
;; thin JNA wrapper providing idiomatic Clojure access to the FFI layer.
;;
;; See ADR-008 in META.scm for the FFI-only binding policy.

(ns proven.core
  "Proven - JNA FFI bindings to Idris 2 verified safety library.

  All computation is performed in Idris 2 via the Zig FFI layer.
  This namespace provides lifecycle management and re-exports all
  sub-namespaces. No logic is reimplemented in Clojure.

  Modules:
    proven.math       - Overflow-checked arithmetic
    proven.string     - UTF-8 validation, HTML/SQL/JS escaping
    proven.path       - Directory traversal detection, filename sanitization
    proven.email      - RFC 5321 email validation
    proven.crypto     - Constant-time comparison, secure random
    proven.json       - JSON syntax validation
    proven.float      - Safe floating-point operations
    proven.header     - HTTP header validation, HSTS building
    proven.cookie     - HTTP cookie injection detection
    proven.checksum   - CRC32 computation and verification
    proven.probability - Probability arithmetic
    proven.angle      - Degree/radian conversion
    proven.ml         - ML activation functions
    proven.calculator - Safe expression evaluation
    proven.hex        - Hex encoding
    proven.http       - URL encoding/decoding
    proven.datetime   - Leap year, days-in-month"
  (:require [proven.native :as n]
            [proven.math]
            [proven.string]
            [proven.path]
            [proven.email]
            [proven.crypto]
            [proven.json]
            [proven.float]
            [proven.header]
            [proven.cookie]
            [proven.checksum]
            [proven.probability]
            [proven.angle]
            [proven.ml]
            [proven.calculator]
            [proven.hex]
            [proven.http]
            [proven.datetime]))

;; Uses proven.native for all FFI calls.

;; ---------------------------------------------------------------------------
;; Lifecycle
;; ---------------------------------------------------------------------------

(defn init
  "Initialize the Proven runtime. Must be called before any other operations.
  Returns true on success."
  []
  (zero? (int (n/call-int "proven_init"))))

(defn deinit
  "Deinitialize the Proven runtime."
  []
  (n/call-void "proven_deinit")
  nil)

(defn initialized?
  "Check if the Proven runtime is initialized."
  []
  (n/call-bool "proven_is_initialized"))

(defn abi-version
  "Get the FFI ABI version."
  []
  (n/call-int "proven_ffi_abi_version"))

(defn version
  "Get the library version as a map {:major :minor :patch}."
  []
  {:major (n/call-int "proven_version_major")
   :minor (n/call-int "proven_version_minor")
   :patch (n/call-int "proven_version_patch")})

(defn version-string
  "Get the library version as a string."
  []
  (let [v (version)]
    (str (:major v) "." (:minor v) "." (:patch v))))

(defn module-count
  "Get the number of modules registered in the library."
  []
  (n/call-int "proven_module_count"))
