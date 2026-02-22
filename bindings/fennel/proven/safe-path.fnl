;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; proven/safe-path.fnl - Safe path operation wrappers for libproven.
;; All computation delegates to the Idris 2 verified core via LuaJIT FFI.
;; Do NOT reimplement any path logic in Fennel.

(local {: lib : bool-result : string-result} (require :proven.ffi))

;; ============================================================================
;; Safe path operations
;; ============================================================================

(fn has-traversal [s]
  "Check if path contains directory traversal sequences. Returns nil on error."
  (bool-result (lib.proven_path_has_traversal s (length s))))

(fn sanitize-filename [s]
  "Sanitize filename by removing dangerous characters. Returns nil on error."
  (string-result (lib.proven_path_sanitize_filename s (length s))))

;; ============================================================================
;; Export
;; ============================================================================

{:has_traversal    has-traversal
 :sanitize_filename sanitize-filename}
