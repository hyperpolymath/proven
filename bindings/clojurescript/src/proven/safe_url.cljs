;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

;; SafeUrl - URL parsing and validation.
;;
;; All functions delegate to libproven via JS interop (transitive FFI).
;; Returns nil on error. NEVER reimplements logic.

(ns proven.safe-url
  "Safe URL parsing and validation.
   All computation delegates to the Idris 2 core via FFI."
  (:require [proven.ffi :as ffi]))


(defn parse-url
  "Parse a URL string into a map of components.
  Returns a map with keys :scheme, :host, :port, :has-port, :path,
  :query, :fragment. Returns nil on parse failure.

  Example:
    (parse-url \"https://example.com:8080/path?q=1#frag\")
    ;=> {:scheme \"https\" :host \"example.com\" :port 8080 ...}"
  [url-str]
  (when-let [lib (ffi/get-lib)]
    (let [result (.proven_url_parse lib url-str (count url-str))]
      (when (ffi/ok? (.-status result))
        (let [c (.-components result)]
          {:scheme   (.-scheme c)
           :host     (.-host c)
           :port     (.-port c)
           :has-port (.-has_port c)
           :path     (.-path c)
           :query    (.-query c)
           :fragment (.-fragment c)})))))
