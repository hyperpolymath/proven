;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeString - JNA wrapper for proven_string_* functions.
;; All computation delegated to libproven; no logic reimplemented here.

(ns proven.string
  "Safe string operations via libproven JNA FFI.

  Provides UTF-8 validation and context-aware escaping (HTML, SQL, JS).
  Every function delegates to Idris 2 verified code. No escaping logic
  is reimplemented in Clojure."
  (:require [proven.native :as n])
  (:import [java.nio.charset StandardCharsets]))

(defn valid-utf8?
  "Check if a byte array is valid UTF-8. Returns boolean or nil on error."
  [^bytes data]
  (let [mem (n/to-native-bytes data)]
    (when mem
      (n/call-bool-result "proven_string_is_valid_utf8" mem (long (alength data))))))

(defn escape-html
  "Escape a string for safe HTML insertion. Returns nil on error."
  [^String input]
  (let [mem (n/to-native-string input)]
    (when mem
      (let [bytes (.getBytes input StandardCharsets/UTF_8)]
        (n/call-string-result "proven_string_escape_html" mem (long (alength bytes)))))))

(defn escape-sql
  "Escape a string for safe SQL insertion. Returns nil on error."
  [^String input]
  (let [mem (n/to-native-string input)]
    (when mem
      (let [bytes (.getBytes input StandardCharsets/UTF_8)]
        (n/call-string-result "proven_string_escape_sql" mem (long (alength bytes)))))))

(defn escape-js
  "Escape a string for safe JavaScript string context. Returns nil on error."
  [^String input]
  (let [mem (n/to-native-string input)]
    (when mem
      (let [bytes (.getBytes input StandardCharsets/UTF_8)]
        (n/call-string-result "proven_string_escape_js" mem (long (alength bytes)))))))
