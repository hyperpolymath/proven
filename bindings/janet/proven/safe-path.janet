# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# proven/safe-path.janet - Safe path operation wrappers for libproven.
# All computation delegates to the Idris 2 verified core via Janet FFI.
# Do NOT reimplement any path logic in Janet.

(import ./ffi :prefix "ffi/")

## ============================================================================
## Safe path operations
## ============================================================================

(defn has-traversal?
  "Check if path contains directory traversal sequences.
  Returns true if traversal detected, nil on error."
  [path]
  (ffi/extract-bool-result (ffi/proven-path-has-traversal path (length path))))

(defn sanitize-filename
  "Sanitize filename by removing dangerous characters. Returns nil on error."
  [filename]
  (ffi/extract-string-result (ffi/proven-path-sanitize-filename filename (length filename))))
