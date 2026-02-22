;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

;; SafeString - Text operations that handle encoding safely.
;;
;; All functions delegate to libproven via JS interop (transitive FFI).
;; Returns nil on error. NEVER reimplements logic.

(ns proven.safe-string
  "Safe string operations: UTF-8 validation and escaping.
   All computation delegates to the Idris 2 core via FFI."
  (:require [proven.ffi :as ffi]))


(defn valid-utf8?
  "Check if a string/buffer is valid UTF-8.
  Returns true/false, or nil on FFI error.

  Example:
    (valid-utf8? \"hello\")  ;=> true"
  [data]
  (when-let [lib (ffi/get-lib)]
    (let [result (.proven_string_is_valid_utf8 lib data (count data))]
      (when (ffi/ok? (.-status result))
        (.-value result)))))


(defn escape-sql
  "Escape a string for SQL (single quotes). Returns nil on error.
  Prefer parameterized queries over string escaping.

  Example:
    (escape-sql \"O'Brien\")  ;=> \"O''Brien\""
  [s]
  (when-let [lib (ffi/get-lib)]
    (let [result (.proven_string_escape_sql lib s (count s))]
      (when (ffi/ok? (.-status result))
        (.-value result)))))


(defn escape-html
  "Escape a string for HTML (prevents XSS). Returns nil on error.

  Example:
    (escape-html \"<script>\")  ;=> \"&lt;script&gt;\""
  [s]
  (when-let [lib (ffi/get-lib)]
    (let [result (.proven_string_escape_html lib s (count s))]
      (when (ffi/ok? (.-status result))
        (.-value result)))))


(defn escape-js
  "Escape a string for JavaScript string literals. Returns nil on error.

  Example:
    (escape-js \"alert('xss')\")  ;=> escaped string"
  [s]
  (when-let [lib (ffi/get-lib)]
    (let [result (.proven_string_escape_js lib s (count s))]
      (when (ffi/ok? (.-status result))
        (.-value result)))))
