; SPDX-License-Identifier: PMPL-1.0-or-later
; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

;; SafeString - Text operations that handle encoding safely.
;;
;; All functions delegate to libproven via FFI.
;; Returns None on error. NEVER reimplements logic.

(import proven.ffi [get-lib ok? encode-str decode-string-result])


(defn is-valid-utf8 [data]
  "Check if byte data is valid UTF-8. Returns True/False, or None on error.

  Args:
    data: bytes or str to validate

  Example:
    (is-valid-utf8 b\"hello\")  ; => True
  "
  (setv b (if (isinstance data str) (.encode data "utf-8") data))
  (setv result (.proven_string_is_valid_utf8 (get-lib) b (len b)))
  (when (ok? result.status)
    (return result.value))
  None)


(defn escape-sql [s]
  "Escape a string for SQL (single quotes). Returns None on error.
  Prefer parameterized queries over string escaping.

  Example:
    (escape-sql \"O'Brien\")  ; => \"O''Brien\"
  "
  (setv #(b n) (encode-str s))
  (setv result (.proven_string_escape_sql (get-lib) b n))
  (decode-string-result result))


(defn escape-html [s]
  "Escape a string for HTML (prevents XSS). Returns None on error.

  Example:
    (escape-html \"<script>\")  ; => \"&lt;script&gt;\"
  "
  (setv #(b n) (encode-str s))
  (setv result (.proven_string_escape_html (get-lib) b n))
  (decode-string-result result))


(defn escape-js [s]
  "Escape a string for JavaScript string literals. Returns None on error.

  Example:
    (escape-js \"alert('xss')\")
  "
  (setv #(b n) (encode-str s))
  (setv result (.proven_string_escape_js (get-lib) b n))
  (decode-string-result result))
