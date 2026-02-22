;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeHeader - JNA wrapper for proven_header_* functions.
;; All computation delegated to libproven; no logic reimplemented here.

(ns proven.header
  "Safe HTTP header operations via libproven JNA FFI."
  (:require [proven.native :as n])
  (:import [java.nio.charset StandardCharsets]))

(defn has-crlf?
  "Check if a header value contains CRLF injection sequences."
  [^String value]
  (let [mem (n/to-native-string value)]
    (when mem
      (let [bytes (.getBytes value StandardCharsets/UTF_8)]
        (n/call-bool-result "proven_header_has_crlf" mem (long (alength bytes)))))))

(defn valid-name?
  "Check if a header name is valid per HTTP spec."
  [^String name]
  (let [mem (n/to-native-string name)]
    (when mem
      (let [bytes (.getBytes name StandardCharsets/UTF_8)]
        (n/call-bool-result "proven_header_is_valid_name" mem (long (alength bytes)))))))

(defn dangerous?
  "Check if a header name is security-dangerous."
  [^String name]
  (let [mem (n/to-native-string name)]
    (when mem
      (let [bytes (.getBytes name StandardCharsets/UTF_8)]
        (n/call-bool-result "proven_header_is_dangerous" mem (long (alength bytes)))))))

(defn render
  "Render a header name-value pair as a proper HTTP header line."
  [^String name ^String value]
  (let [name-bytes (.getBytes name StandardCharsets/UTF_8)
        value-bytes (.getBytes value StandardCharsets/UTF_8)
        name-mem (n/to-native-bytes name-bytes)
        value-mem (n/to-native-bytes value-bytes)]
    (when (and name-mem value-mem)
      (n/call-string-result "proven_header_render"
                            name-mem (long (alength name-bytes))
                            value-mem (long (alength value-bytes))))))

(defn build-hsts
  "Build an HSTS header value."
  [^long max-age ^boolean include-subdomains ^boolean preload]
  (n/call-string-result "proven_header_build_hsts"
                        (long max-age) (boolean include-subdomains) (boolean preload)))
