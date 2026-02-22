;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeJson - JNA wrapper for proven_json_* functions.
;; All computation delegated to libproven; no logic reimplemented here.

(ns proven.json
  "Safe JSON validation via libproven JNA FFI."
  (:require [proven.native :as n])
  (:import [java.nio.charset StandardCharsets]))

(defn valid?
  "Check if a string contains valid JSON. Returns boolean or nil."
  [^String json]
  (let [mem (n/to-native-string json)]
    (when mem
      (let [bytes (.getBytes json StandardCharsets/UTF_8)]
        (n/call-bool-result "proven_json_is_valid" mem (long (alength bytes)))))))
