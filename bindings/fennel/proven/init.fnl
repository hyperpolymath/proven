;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; proven/init.fnl - Main module for Fennel bindings to libproven.
;; All computation is performed in Idris 2 via the Zig FFI layer.
;; This module is a thin wrapper; it does NOT reimplement any logic.
;;
;; Fennel compiles to Lua and uses LuaJIT FFI for native C interop.

(local {: lib : ffi} (require :proven.ffi))
(local safe-math (require :proven.safe-math))
(local safe-string (require :proven.safe-string))
(local safe-path (require :proven.safe-path))
(local safe-email (require :proven.safe-email))
(local safe-url (require :proven.safe-url))
(local safe-crypto (require :proven.safe-crypto))
(local safe-json (require :proven.safe-json))

;; ============================================================================
;; Runtime lifecycle
;; ============================================================================

(fn init []
  "Initialize the Proven runtime (Idris 2 + Zig FFI).
Must be called before any other proven function.
Returns true on success, false on failure."
  (= (lib.proven_init) 0))

(fn deinit []
  "Shut down the Proven runtime."
  (lib.proven_deinit))

(fn is-initialized []
  "Return true if the Proven runtime is initialized."
  (lib.proven_is_initialized))

(fn abi-version []
  "Return the FFI ABI version number."
  (tonumber (lib.proven_ffi_abi_version)))

(fn version []
  "Return the libproven version as a string."
  (string.format "%d.%d.%d"
    (lib.proven_version_major)
    (lib.proven_version_minor)
    (lib.proven_version_patch)))

(fn module-count []
  "Return the number of modules in libproven."
  (tonumber (lib.proven_module_count)))

;; ============================================================================
;; Module table
;; ============================================================================

{:_VERSION     "0.9.0"
 :_DESCRIPTION "Fennel FFI binding to libproven - formally verified safety library"
 :_LICENSE     "PMPL-1.0-or-later"
 : init
 : deinit
 :is_initialized is-initialized
 :abi_version    abi-version
 : version
 :module_count   module-count
 :SafeMath       safe-math
 :SafeString     safe-string
 :SafePath       safe-path
 :SafeEmail      safe-email
 :SafeUrl        safe-url
 :SafeCrypto     safe-crypto
 :SafeJson       safe-json}
