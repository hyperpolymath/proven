;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafeEmail - JNA wrapper for proven_email_* functions.
;; All computation delegated to libproven; no logic reimplemented here.

(ns proven.email
  "Safe email validation via libproven JNA FFI."
  (:require [proven.native :as n])
  (:import [java.nio.charset StandardCharsets]))

(defn valid?
  "Check if an email address is valid (RFC 5321). Returns boolean or nil."
  [^String email]
  (let [mem (n/to-native-string email)]
    (when mem
      (let [bytes (.getBytes email StandardCharsets/UTF_8)]
        (n/call-bool-result "proven_email_is_valid" mem (long (alength bytes)))))))
