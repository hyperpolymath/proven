# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# proven/init.janet - Main module for Janet bindings to libproven.
# All computation is performed in Idris 2 via the Zig FFI layer.
# This module is a thin wrapper; it does NOT reimplement any logic.
#
# Janet bindings use the ffi/ module (ffi/native, ffi/defbind) for
# direct C ABI interop with libproven.

(import ./ffi :prefix "ffi/")
(import ./safe-math :as safe-math)
(import ./safe-string :as safe-string)
(import ./safe-path :as safe-path)
(import ./safe-email :as safe-email)
(import ./safe-url :as safe-url)
(import ./safe-crypto :as safe-crypto)
(import ./safe-json :as safe-json)

## ============================================================================
## Module metadata
## ============================================================================

(def version "0.9.0")
(def description "Janet FFI binding to libproven - formally verified safety library")
(def license "PMPL-1.0-or-later")

## ============================================================================
## Runtime lifecycle
## ============================================================================

(defn init
  "Initialize the Proven runtime (Idris 2 + Zig FFI).
  Must be called before any other proven function.
  Returns true on success, nil on failure."
  []
  (when (= (ffi/proven-init) 0) true))

(defn deinit
  "Shut down the Proven runtime."
  []
  (ffi/proven-deinit))

(defn initialized?
  "Return true if the Proven runtime is initialized."
  []
  (ffi/proven-is-initialized))

(defn abi-version
  "Return the FFI ABI version number."
  []
  (ffi/proven-ffi-abi-version))

(defn library-version
  "Return the libproven version as a string."
  []
  (string/format "%d.%d.%d"
    (ffi/proven-version-major)
    (ffi/proven-version-minor)
    (ffi/proven-version-patch)))

(defn module-count
  "Return the number of modules in libproven."
  []
  (ffi/proven-module-count))
