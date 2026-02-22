;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeHTTP - JNA wrapper for proven_http_* functions.
;; All computation delegated to libproven; no logic reimplemented here.

(ns proven.http
  "Safe HTTP URL encoding/decoding via libproven JNA FFI."
  (:require [proven.native :as n])
  (:import [java.nio.charset StandardCharsets]))

(defn url-encode
  "URL-encode a string (percent-encoding). Returns nil on error."
  [^String input]
  (let [mem (n/to-native-string input)]
    (when mem
      (let [bytes (.getBytes input StandardCharsets/UTF_8)]
        (n/call-string-result "proven_http_url_encode" mem (long (alength bytes)))))))

(defn url-decode
  "URL-decode a percent-encoded string. Returns nil on error."
  [^String input]
  (let [mem (n/to-native-string input)]
    (when mem
      (let [bytes (.getBytes input StandardCharsets/UTF_8)]
        (n/call-string-result "proven_http_url_decode" mem (long (alength bytes)))))))
