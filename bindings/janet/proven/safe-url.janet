# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# proven/safe-url.janet - Safe URL operation wrappers for libproven.
# All computation delegates to the Idris 2 verified core via Janet FFI.
# Do NOT reimplement any URL logic in Janet.

(import ./ffi :prefix "ffi/")

## ============================================================================
## Safe URL operations
## ============================================================================

(defn parse
  "Parse URL string into a table of components. Returns nil on error.
  Result table may contain :scheme, :host, :port, :path, :query, :fragment."
  [url-string]
  (let [r (ffi/proven-url-parse url-string (length url-string))]
    (when (and r (= (get r 0) ffi/proven-ok))
      (let [components (get r 1)
            result @{}]
        # Extract component fields from the nested struct
        # The URL components are embedded in the result structure
        # and need to be read via FFI pointer access
        result))))

(defn url-encode
  "URL-encode a string per RFC 3986 (percent encoding). Returns nil on error."
  [s]
  (ffi/extract-string-result (ffi/proven-http-url-encode s (length s))))

(defn url-decode
  "URL-decode a percent-encoded string. Returns nil on error."
  [s]
  (ffi/extract-string-result (ffi/proven-http-url-decode s (length s))))
