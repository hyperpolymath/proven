;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeHex - JNA wrapper for proven_hex_* functions.
;; All computation delegated to libproven; no logic reimplemented here.

(ns proven.hex
  "Safe hexadecimal encoding via libproven JNA FFI."
  (:require [proven.native :as n]))

(defn encode
  "Encode raw bytes to hexadecimal string."
  ([^bytes data] (encode data false))
  ([^bytes data ^boolean uppercase]
   (let [mem (n/to-native-bytes data)]
     (when mem
       (n/call-string-result "proven_hex_encode"
                             mem (long (alength data)) (boolean uppercase))))))
