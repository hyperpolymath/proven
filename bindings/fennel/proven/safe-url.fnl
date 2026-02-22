;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; proven/safe-url.fnl - Safe URL operation wrappers for libproven.
;; All computation delegates to the Idris 2 verified core via LuaJIT FFI.
;; Do NOT reimplement any URL logic in Fennel.

(local {: lib : ffi : string-result} (require :proven.ffi))

;; ============================================================================
;; Safe URL operations
;; ============================================================================

(fn parse [s]
  "Parse URL string into a table of components. Returns nil on error.
Result table may contain: scheme, host, port, path, query, fragment."
  (let [r (lib.proven_url_parse s (length s))]
    (when (= r.status 0)
      (let [c r.components
            result {}]
        (when (and (not= c.scheme nil) (> c.scheme_len 0))
          (tset result :scheme (ffi.string c.scheme c.scheme_len)))
        (when (and (not= c.host nil) (> c.host_len 0))
          (tset result :host (ffi.string c.host c.host_len)))
        (when c.has_port
          (tset result :port (tonumber c.port)))
        (when (and (not= c.path nil) (> c.path_len 0))
          (tset result :path (ffi.string c.path c.path_len)))
        (when (and (not= c.query nil) (> c.query_len 0))
          (tset result :query (ffi.string c.query c.query_len)))
        (when (and (not= c.fragment nil) (> c.fragment_len 0))
          (tset result :fragment (ffi.string c.fragment c.fragment_len)))
        (lib.proven_url_free r.components)
        result))))

(fn url-encode [s]
  "URL-encode a string per RFC 3986. Returns nil on error."
  (string-result (lib.proven_http_url_encode s (length s))))

(fn url-decode [s]
  "URL-decode a percent-encoded string. Returns nil on error."
  (string-result (lib.proven_http_url_decode s (length s))))

;; ============================================================================
;; Export
;; ============================================================================

{: parse
 :url_encode url-encode
 :url_decode url-decode}
