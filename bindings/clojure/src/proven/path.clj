;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
;;
;; SafePath - JNA wrapper for proven_path_* functions.
;; All computation delegated to libproven; no logic reimplemented here.

(ns proven.path
  "Safe filesystem path operations via libproven JNA FFI.

  Provides traversal detection and filename sanitization.
  Every function delegates to Idris 2 verified code."
  (:require [proven.native :as n])
  (:import [java.nio.charset StandardCharsets]))

(defn has-traversal?
  "Check if a path contains directory traversal sequences. Returns boolean or nil."
  [^String path]
  (let [mem (n/to-native-string path)]
    (when mem
      (let [bytes (.getBytes path StandardCharsets/UTF_8)]
        (n/call-bool-result "proven_path_has_traversal" mem (long (alength bytes)))))))

(defn sanitize-filename
  "Sanitize a filename by removing dangerous characters. Returns nil on error."
  [^String filename]
  (let [mem (n/to-native-string filename)]
    (when mem
      (let [bytes (.getBytes filename StandardCharsets/UTF_8)]
        (n/call-string-result "proven_path_sanitize_filename" mem (long (alength bytes)))))))
