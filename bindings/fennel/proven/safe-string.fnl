;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; proven/safe-string.fnl - Safe string operation wrappers for libproven.
;; All computation delegates to the Idris 2 verified core via LuaJIT FFI.
;; Do NOT reimplement any string logic in Fennel.

(local {: lib : bool-result : string-result} (require :proven.ffi))

;; ============================================================================
;; Safe string operations
;; ============================================================================

(fn is-valid-utf8 [s]
  "Check if string contains valid UTF-8. Returns nil on error."
  (bool-result (lib.proven_string_is_valid_utf8 s (length s))))

(fn escape-sql [s]
  "Escape string for SQL (single quotes). Returns nil on error."
  (string-result (lib.proven_string_escape_sql s (length s))))

(fn escape-html [s]
  "Escape string for HTML (prevents XSS). Returns nil on error."
  (string-result (lib.proven_string_escape_html s (length s))))

(fn escape-js [s]
  "Escape string for JavaScript string literals. Returns nil on error."
  (string-result (lib.proven_string_escape_js s (length s))))

;; ============================================================================
;; Export
;; ============================================================================

{:is_valid_utf8 is-valid-utf8
 :escape_sql    escape-sql
 :escape_html   escape-html
 :escape_js     escape-js}
