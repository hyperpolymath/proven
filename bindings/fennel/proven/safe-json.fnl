;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; proven/safe-json.fnl - Safe JSON validation wrappers for libproven.
;; All computation delegates to the Idris 2 verified core via LuaJIT FFI.
;; Do NOT reimplement any JSON logic in Fennel.

(local {: lib : bool-result} (require :proven.ffi))

;; ============================================================================
;; JSON type name lookup
;; ============================================================================

(local type-names
  {0  :null
   1  :boolean
   2  :number
   3  :string
   4  :array
   5  :object
   -1 :invalid})

;; ============================================================================
;; Safe JSON operations
;; ============================================================================

(fn is-valid [s]
  "Check if string is valid JSON. Returns nil on error."
  (bool-result (lib.proven_json_is_valid s (length s))))

(fn get-type [s]
  "Get JSON value type at root level. Returns type name string or \"invalid\"."
  (let [t (lib.proven_json_get_type s (length s))]
    (or (. type-names t) :invalid)))

;; ============================================================================
;; Export
;; ============================================================================

{:is_valid is-valid
 :get_type get-type}
