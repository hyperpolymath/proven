;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeCookie - JNA wrapper for proven_cookie_* functions.
;; All computation delegated to libproven; no logic reimplemented here.

(ns proven.cookie
  "Safe HTTP cookie operations via libproven JNA FFI."
  (:require [proven.native :as n])
  (:import [java.nio.charset StandardCharsets]))

(defn has-injection?
  "Check if a cookie value contains injection characters."
  [^String value]
  (let [mem (n/to-native-string value)]
    (when mem
      (let [bytes (.getBytes value StandardCharsets/UTF_8)]
        (n/call-bool-result "proven_cookie_has_injection" mem (long (alength bytes)))))))

(defn valid-name?
  "Validate a cookie name."
  [^String name]
  (let [mem (n/to-native-string name)]
    (when mem
      (let [bytes (.getBytes name StandardCharsets/UTF_8)]
        (n/call-bool-result "proven_cookie_validate_name" mem (long (alength bytes)))))))

(defn valid-value?
  "Validate a cookie value."
  [^String value]
  (let [mem (n/to-native-string value)]
    (when mem
      (let [bytes (.getBytes value StandardCharsets/UTF_8)]
        (n/call-bool-result "proven_cookie_validate_value" mem (long (alength bytes)))))))

(defn get-prefix
  "Get the cookie prefix type (__Host-, __Secure-, or none)."
  [^String name]
  (let [mem (n/to-native-string name)]
    (when mem
      (let [bytes (.getBytes name StandardCharsets/UTF_8)]
        (n/call-int-result "proven_cookie_get_prefix" mem (long (alength bytes)))))))
