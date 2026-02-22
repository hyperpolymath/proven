# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# proven/safe-string.janet - Safe string operation wrappers for libproven.
# All computation delegates to the Idris 2 verified core via Janet FFI.
# Do NOT reimplement any string logic in Janet.

(import ./ffi :prefix "ffi/")

## ============================================================================
## Safe string operations
## ============================================================================

(defn valid-utf8?
  "Check if string contains valid UTF-8. Returns nil on error."
  [s]
  (ffi/extract-bool-result (ffi/proven-string-is-valid-utf8 s (length s))))

(defn escape-sql
  "Escape string for SQL (single quotes). Returns nil on error.
  NOTE: Prefer parameterized queries over string escaping."
  [s]
  (ffi/extract-string-result (ffi/proven-string-escape-sql s (length s))))

(defn escape-html
  "Escape string for HTML (prevents XSS). Returns nil on error."
  [s]
  (ffi/extract-string-result (ffi/proven-string-escape-html s (length s))))

(defn escape-js
  "Escape string for JavaScript string literals. Returns nil on error."
  [s]
  (ffi/extract-string-result (ffi/proven-string-escape-js s (length s))))
