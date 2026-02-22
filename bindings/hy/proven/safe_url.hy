; SPDX-License-Identifier: PMPL-1.0-or-later
; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

;; SafeUrl - URL parsing and validation.
;;
;; All functions delegate to libproven via FFI.
;; Returns None on error. NEVER reimplements logic.

(import ctypes)
(import proven.ffi [get-lib ok? encode-str UrlComponents])


(defn parse-url [url-str]
  "Parse a URL string into a dictionary of components.
  Returns a dict with keys: scheme, host, port, has-port, path, query, fragment.
  Returns None on parse failure.

  Example:
    (parse-url \"https://example.com:8080/path?q=1#frag\")
    ; => {\"scheme\" \"https\" \"host\" \"example.com\" \"port\" 8080 ...}
  "
  (setv #(b n) (encode-str url-str))
  (setv lib (get-lib))
  (setv result (.proven_url_parse lib b n))
  (when (!= result.status 0)
    (return None))
  (setv c result.components)
  (setv parsed
    {"scheme"   (if c.scheme (.decode c.scheme "utf-8" :errors "replace") "")
     "host"     (if c.host (.decode c.host "utf-8" :errors "replace") "")
     "port"     (int c.port)
     "has-port" (bool c.has_port)
     "path"     (if c.path (.decode c.path "utf-8" :errors "replace") "")
     "query"    (if c.query (.decode c.query "utf-8" :errors "replace") "")
     "fragment" (if c.fragment (.decode c.fragment "utf-8" :errors "replace") "")})
  ;; Free the URL components
  (try
    (.proven_url_free lib (ctypes.byref c))
    (except [Exception]))
  parsed)
